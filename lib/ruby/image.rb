class Image
  PNG_DIR = 'png'

  def self.from_android(filename)
    # `#{ADB} shell screencap -p | perl -pe '#{regex}' > #{png_filepath(filename)}`
    new(filename)
  end

  def self.png_filepath(filename)
    File.join(PNG_DIR, filename)
  end

  def initialize(filename)
    @filename = filename
  end

  def png_filepath(filename)
    self.class.png_filepath(filename)
  end

  def small_filename(filename)
    "small-#{filename}"
  end

  def cropped_filename(filename)
    "cropped-#{filename}"
  end

  def pixelated_filename(filename)
    "pixelated-#{filename}"
  end

  def prefixed_filename(prefix, filename)
    "#{prefix}-#{filename}"
  end

  def resize(percent)
    small_file = small_filename(@filename)
    `convert -resize #{percent}% #{png_filepath(@filename)} #{png_filepath(small_file)}`
    Image.new(small_file)
  end

  def crop(minrow, maxrow)
    png = ChunkyPNG::Image.new(inner.width, maxrow-minrow, ChunkyPNG::Color.rgba(0, 255, 0, 255))

    (0..inner.width-1).each do |col|
      (minrow..maxrow-1).each do |row|
        png[col, row - minrow] = inner[col, row]
      end
    end

    png.save(png_filepath(cropped_filename(@filename)))
    Image.new(cropped_filename(@filename))
  end

  def pixelize(ascii_board_dimensions, pixel_dimensions, scheme_filepath)
    png = ChunkyPNG::Image.new(inner.width, inner.height, ChunkyPNG::Color.rgba(0, 255, 0, 255))
    scm = File.open(scheme_filepath, 'w') do |f|
      f.write("(define screen '#(\n")

      (0..ascii_board_dimensions.height-1).each do |ascii_row|
        f.write("#(")
        (0..ascii_board_dimensions.width-1).each do |ascii_col|
          avg_color = nil
          each_pixel_of_ascii_tile(ascii_col, ascii_row, pixel_dimensions) do |col, row, avg|
            avg_color = avg
            png[col, row] = ChunkyPNG::Color.rgba(avg.r, avg.g, avg.b, 255)
          end
          f.write("(#{avg_color.r} #{avg_color.g} #{avg_color.b}) ")
        end
        f.write(")\n")
      end

      f.write("))")
    end

    png.save(png_filepath(pixelated_filename(@filename)))
    Image.new(pixelated_filename(@filename))
  end

  def each_pixel_of_ascii_tile(ascii_col, ascii_row, pixel_dimensions)
    mincol = (ascii_col*pixel_dimensions.width).floor
    minrow = (ascii_row*pixel_dimensions.height).floor
    maxcol = (mincol + pixel_dimensions.width).floor
    maxrow = (minrow + pixel_dimensions.height).floor

    avg = avg_rect(mincol, maxcol, minrow, maxrow)

    (minrow..maxrow).each do |row|
      (mincol..maxcol).each do |col|
        yield col, row, avg
      end
    end
  end

  def avg_rect(mincol, maxcol, minrow, maxrow)
    avg = AverageColor.new
    (minrow..maxrow).each do |row|
      (mincol..maxcol).each do |col|
        avg.sample(inner[col, row])
      end
    end
    avg
  end

  def char_dimensions_in_pixels(ascii_dimensions)
    Dimensions.new(
      width.to_f / ascii_dimensions.width,
      height.to_f / ascii_dimensions.height
    )
  end

  def ratio_to_pixels(col_ratio, row_ratio)
    # screen_dimensions = Dimensions.new(1080, 1920)
    screen_dimensions = Dimensions.new(inner.width, inner.height)
    [
      col_ratio * screen_dimensions.width,
      row_ratio * screen_dimensions.height
    ].map(&:to_i).tap do |(col, row)|
      draw_dot(col, row, 'tapped')
    end
  end

  def draw_dot(col, row, filename_prefix)
    image = ChunkyPNG::Image.from_file(png_filepath(@filename))
    [[0, 1], [1, 1], [1, 0], [0, 0]].each do |(a, b)|
      image[col+a, row+b] = ChunkyPNG::Color.rgba(255, 0, 0, 255)
    end
    output_filename = prefixed_filename(filename_prefix, @filename)
    image.save(png_filepath(output_filename))
    Image.new(output_filename)
  end

  def width
    inner.width
  end

  def height
    inner.height
  end

  def inner
    @inner ||= ChunkyPNG::Image.from_file(png_filepath(@filename))
  end
end