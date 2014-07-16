class Screen
  IMAGE_FILENAME = 'screen.png'
  SCHEME_BOARDS_PATH = 'lib/scheme/boards.scm'
  SCHEME_SCREEN_PATH = 'lib/scheme/screen.scm'
  ADB = "/home/kajboj/code/adt/adt-bundle-linux-x86-20131030/sdk/platform-tools/adb"
  REGEX = 's/\x0D\x0A/\x0A/g'
  CROP = {top: 18, bottom: -101}
  TAP_CORRECTION = {row: -0.05}

  def self.from_android
    image_filepath = Image.png_filepath(IMAGE_FILENAME)
    `#{ADB} shell screencap -p | perl -pe '#{REGEX}' > #{image_filepath}`

    image = Image.from_android(IMAGE_FILENAME)
    new(image)
  end

  def initialize(image)
    @image = image.resize(25)
  end

  def to_scheme_rgb
    @cropped = @image.crop(CROP[:top], @image.height+CROP[:bottom])

    ascii_board = AsciiBoard.new(SCHEME_BOARDS_PATH)

    pixel_dimensions = @cropped.char_dimensions_in_pixels(ascii_board.dimensions)

    @cropped.pixelize(
      ascii_board.dimensions,
      pixel_dimensions,
      SCHEME_SCREEN_PATH)
  end

  def tap(ratio_col, ratio_row)
    col, row = unshrink(
      *uncrop(
        *@cropped.ratio_to_pixels(
          *correct_tap(ratio_col, ratio_row))))
    puts col, row

    `#{ADB} shell input tap #{col} #{row}`
  end

  def uncrop(col, row)
    [col, row+CROP[:top]]
  end

  def unshrink(col, row)
    [col*4, row*4]
  end

  def correct_tap(col, row)
    [col, row+TAP_CORRECTION[:row]]
  end
end