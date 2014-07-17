class Leap < HopliteMove
  def execute(screen)
    screen.special_skill('leap')
    screen.tap_ratio(*coords)
  end
end