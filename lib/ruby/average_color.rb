class AverageColor
  def initialize
    @r, @g, @b = 0, 0, 0
    @count = 0
  end
  def sample(i)
    @r += ChunkyPNG::Color.r(i)
    @g += ChunkyPNG::Color.g(i)
    @b += ChunkyPNG::Color.b(i)
    @count += 1
  end
  def r; @r / @count; end
  def g; @g / @count; end
  def b; @b / @count; end
end