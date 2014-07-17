class ByOne < HopliteMove
  def execute(screen)
    screen.tap_ratio(*coords)
  end
end