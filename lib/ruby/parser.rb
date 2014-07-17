module Parser
  TYPE_MAP = {
    'by-one' => Leap,
    'leap'   => Leap
  }

  def self.parse_move(output)
    last_line = output.split("\n").last

    unless last_line =~ /\Asuccess.*/
      raise HopliteError.new('no success message from scheme')
    else
      last_line =~ /success: (.*?), ([0-9\/]+) ([0-9\/]+)/

      move_type = $1
      col_s = $2
      row_s = $3

      coords = [col_s, row_s].map { |s| parse_rational(s) }

      new_move(move_type, coords)
    end
  end

  def self.new_move(type, coords)
    TYPE_MAP[type].new(coords)
  end

  def self.parse_rational(s)
    a = s.split("/").compact.map(&:to_f)
    a[1] ? a[0]/a[1] : a[0]
  end
end