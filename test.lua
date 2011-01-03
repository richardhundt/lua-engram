local engram = require "engram".engram

local Point = { }
Point.__index = { }
Point.new = function(class)
   return setmetatable({ x = 1, y = 2 } , class)
end
Point.__index.move = function(self, x, y)
   self.x = x
   self.y = y
end

local func = engram({
   point = Point:new(),
   answer = 42,
   deep = { question = "petunias?" },
}, _G)

local data = func()
data.point:move(11,22)
assert(data.point.x == 11)
assert(data.point.y == 22)
assert(data.answer == 42)
assert(data.deep.question == "petunias?")

print "OK"
