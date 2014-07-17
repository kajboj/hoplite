class HopliteMove
  attr_reader :coords

  def initialize(coords)
    @coords = coords
  end

  def execute
    raise 'abstract method'
  end
end