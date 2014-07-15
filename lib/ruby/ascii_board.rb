class AsciiBoard
  def initialize(filepath)
    boards = File.read('lib/scheme/boards.scm')
    empty_board = boards.scan(/define empty-board "(.*?)"/m).first.first
    @board = empty_board.gsub("\\\\", "\\")[1..-1]
  end

  def dimensions
    lines = @board.split("\n")
    height = lines.size
    width = lines.max_by(&:length).size
    Dimensions.new(width, height)
  end
end