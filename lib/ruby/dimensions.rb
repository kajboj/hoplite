class Dimensions
  attr_reader :width, :height
  def initialize(width, height)
    @width, @height = width, height
  end
  def to_s
    "(#{width}x#{height})"
  end
end