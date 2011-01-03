require "pack"

local getfenv, getinfo = debug.getfenv, debug.getinfo

local bpack = string.pack

local _OP = 2^0
local _A  = 2^6
local _B  = 2^(6 + 8 + 9)
local _C  = 2^(6 + 8)
local _Bx = 2^(6 + 8)

local OP_MOVE      = 0
local OP_LOADK     = 1
local OP_GETGLOBAL = 5
local OP_GETTABLE  = 6
local OP_SETTABLE  = 9
local OP_NEWTABLE  = 10
local OP_CALL      = 28
local OP_TAILCALL  = 29
local OP_RETURN    = 30
local OP_CLOSURE   = 36

local OP_iABC  = 0
local OP_iABx  = 1

local OP_MODES = {
   [OP_MOVE     ] = 0,
   [OP_LOADK    ] = 1,
   [OP_GETGLOBAL] = 1,
   [OP_GETTABLE ] = 0,
   [OP_SETTABLE ] = 0,
   [OP_NEWTABLE ] = 0,
   [OP_CALL     ] = 0,
   [OP_TAILCALL ] = 0,
   [OP_RETURN   ] = 0,
   [OP_CLOSURE  ] = 1,
}

-- const types
local TNIL = 0
local TBIT = 1
local TNUM = 3
local TSTR = 4
local VNIL = { }

local strdump,strsub,tostring,type = string.dump,string.sub,tostring,type

local Common = { } do
   local class = Common
   class.__index = class

   local samp = strdump(function() end)
   local header = strsub(samp, 1, 12)

   function class.new(class, name)
      local self = setmetatable({
         opcode = { };
         kcache = { };
         consts = { };
         protos = { };
         locals = { };
         vstack = 2;
         vcount = 0;
         source = name;
      }, class)
      local seen = self:alloc()

      self:emit(OP_NEWTABLE, seen, 16, 0)
      self.seenreg = seen
      self.seenidx = 0
      self.seentbl = { }

      return self
   end

   function class.alloc(self)
      local index = self.vstack
      self.vstack = index + 1
      if self.vstack > self.vcount then
         self.vcount = self.vstack
      end
      return index
   end

   function class.emit(self, o, a, b, c)
      a = a or 0
      b = b or 0
      local m = OP_MODES[o]
      local opcode = self.opcode
      if m == OP_iABC then
         c = c or 0
         opcode[#opcode + 1] = o*_OP + a*_A + b*_B + c*_C
      elseif m == OP_iABx then
         opcode[#opcode + 1] = o*_OP + a*_A + b*_Bx
      else
         error("unknown op mode "..tostring(m).." for: "..OP[o])
      end
   end

   function class.bake(self)
      -- bake prologue
      local source
      if self.source then
         source = bpack("a", self.source.."\0")
      else
         source = bpack("a", "")
      end

      -- mandatory final return
      self:emit(OP_RETURN, 0, 1, 0)

      local lndefn = bpack('=I', 0)
      local lnlast = bpack('=I', 0)
      local nupval = bpack('b', 0)
      local nparam = bpack('b', 0)
      local vararg = bpack('b', 2)
      local vcount = bpack('b', self.vcount)
      local buffer = {source, lndefn, lnlast, nupval, nparam, vararg, vcount}

      -- bake opcode
      buffer[#buffer + 1] = bpack("=I", #self.opcode)
      for i,op in ipairs(self.opcode) do
         buffer[#buffer + 1] = bpack("=I", op)
      end

      -- bake consts
      buffer[#buffer + 1] = bpack("=I", #self.consts)
      for index,value in ipairs(self.consts) do
         local vtype = type(value)
         local ctype, const
         if value == VNIL then
            const = bpack('b', TNIL)
         elseif vtype == "boolean" then
            const = bpack('bb', TBIT, value and 1 or 0)
         elseif vtype == "number" then
            const = bpack('bn', TNUM, value)
         elseif vtype == "string" then
            const = bpack('ba', TSTR, value.."\0")
         end
         buffer[#buffer + 1] = const
      end

      -- bake protos
      buffer[#buffer + 1] = bpack("=I", #self.protos)
      for i,dump in ipairs(self.protos) do
         buffer[#buffer + 1] = dump
      end

      -- bake debug stuff, lninfo, locals, upvals
      buffer[#buffer + 1] = bpack("=I", 0)
      buffer[#buffer + 1] = bpack("=I", 0)
      buffer[#buffer + 1] = bpack("=I", 0)

      return header..table.concat(buffer, '')
   end

   function class.const(self, value)
      if value == nil then value = VNIL end
      if self.kcache[value] then
         return self.kcache[value]
      end

      local index = #self.consts
      self.kcache[value] = index
      self.consts[index + 1] = value

      return index
   end

   function class.save_item(self, obj)
      if self.locals[obj] then
         return self.locals[obj]
      end
      local reg = self:alloc()
      if self.seentbl[obj] then
         local key = self.seentbl[obj]
         self:emit(OP_GETTABLE, reg, self.seenreg, self:const(key) + 256)
         return reg
      end
      local t = type(obj)
      if t == "number" then
         return self:save_const(obj, reg)
      elseif t == "boolean" then
         return self:save_const(obj, reg)
      elseif t == "string" then
         return self:save_const(obj, reg)
      end

      self.locals[obj] = reg
      if t == "table" then
         self:save_table(obj, reg)
      elseif t == "function" then
         self:save_function(obj, reg)
      elseif t == "userdata" then
         self:save_userdata(obj, reg)
      elseif t == "thread" then
         -- in the unlikely event that this is ever implemented,
         self:save_thread(obj, reg)
      end

      local idx = self.seenidx + 1
      self.seenidx = idx
      self.seentbl[obj] = idx

      local key = self:save_item(idx)
      self.vstack = key
      self:emit(OP_SETTABLE, self.seenreg, key, reg)
      self.locals[obj] = nil

      return reg
   end

   function class.save_const(self, obj, reg)
      self:emit(OP_LOADK, reg, self:const(obj))
      return reg
   end

   function class.save_table(self, obj, reg)
      self:emit(OP_NEWTABLE, reg, 0, 0)
      local free = self.vstack
      for k,v in next, obj, nil do
         local kreg = self:save_item(k)
         local vreg = self:save_item(v)
         self:emit(OP_SETTABLE, reg, kreg, vreg)
         self.vstack = free
      end
      return reg
   end

   function class.save_function(self, obj, reg)
      local dump = strdump(obj)
      local body = strsub(dump, 13)
      self.protos[#self.protos + 1] = body
      self:emit(OP_CLOSURE, reg, #self.protos - 1)
      return reg
   end

   function class.save_thread(self, obj, reg)
      return self:save_const(nil, reg)
   end

   function class.save_userdata(self, obj, reg)
      local meta = debug.getmetatable(obj)
      if meta and meta.__persist then
         local func = meta.__persist(obj)
         return self:save_function(func, reg)
      else
         return self:save_const(nil, reg)
      end
   end
end

local LightContext = setmetatable({ }, { __index = Common }) do
   local class = LightContext
   local super = Common
   class.__index = class

   function class.new(class)
      local self = super.new(class)
      return self
   end

   function class.save(self, data, func)
      local dreg = self:save_item(data)
      if func then
         local freg = self:save_function(func, self:alloc())
         local arg1 = self:alloc()
         self:emit(OP_MOVE, arg1, dreg)
         self:emit(OP_TAILCALL, freg, 2, 0)
         self:emit(OP_RETURN, freg, 0, 0)
      else
         self:emit(OP_RETURN, dreg, 2, 0)
      end
      return self:bake()
   end

   function class.save_thread(self, obj, reg)
      return self:save_const(nil, reg)
   end
   function class.save_userdata(self, obj, reg)
      return self:save_const(nil, reg)
   end

end

local HeavyContext = setmetatable({ }, { __index = Common }) do
   local class = HeavyContext
   local super = Common
   class.__index = class

   function class.new(class, env)
      local self = super.new(class)
      self.global = { }
      for k,v in next, env, nil do
         self.global[v] = k
      end

      local dbug = self:alloc()
      self.locals[debug] = dbug
      self:emit(OP_GETGLOBAL, dbug, self:const("debug"))

      local dest = self:alloc()
      self.locals[debug.setmetatable] = dest
      self:emit(OP_GETTABLE, dest, dbug, self:const("setmetatable") + 256)
      self.setmeta = dest

      local dest = self:alloc()
      self.locals[debug.setupvalue] = dest
      self:emit(OP_GETTABLE, dest, dbug, self:const("setupvalue") + 256)
      self.setuval = dest

      return self
   end

   function class.save(self, obj)
      local base = self:save_item(obj)
      self:emit(OP_RETURN, base, 2, 0)
      return self:bake()
   end

   function class.save_item(self, obj)
      if self.global[obj] ~= nil then
         local reg = self:alloc()
         local key = self.global[obj]
         self:emit(OP_GETGLOBAL, reg, self:const(key))
         return reg
      end
      return super.save_item(self, obj)
   end

   local getupval, getinfo = debug.getupvalue, debug.getinfo
   function class.save_function(self, obj, reg)
      super.save_function(self, obj, reg)
      if getupval(obj, 1) then
         local base = self.vstack
         local arg1 = base + 1
         local arg2 = base + 2
         local arg3 = base + 3

         self:emit(OP_MOVE, base, self.setuval)
         self:emit(OP_MOVE, arg1, reg)

         local idx, got, val = 0
         while true do
            idx = idx + 1
            got,val = getupval(obj, idx)
            if not got then break end
            self.vstack = arg2
            self:save_item(idx)
            self:save_item(val)
            self:emit(OP_CALL, base, 3, 1)
         end
         self.vstack = base
      end

      return reg
   end

   function class.save_table(self, obj, reg)
      super.save_table(self, obj, reg)
      local meta = debug.getmetatable(obj)
      if meta ~= nil then
         local mreg = self:save_item(meta)
         local base = self:alloc()
         local arg1 = self:alloc()
         local arg2 = self:alloc()
         self:emit(OP_MOVE, base, self.setmeta)
         self:emit(OP_MOVE, arg1, reg)
         self:emit(OP_MOVE, arg2, mreg)
         self:emit(OP_CALL, base, 3, 1)
         self.vstack = mreg
      end
      return reg
   end

end

local function engram(obj, env)
   local ctx = env and HeavyContext:new(env) or LightContext:new()
   return loadstring(ctx:save(obj))
end

return {
   engram = engram;
}

