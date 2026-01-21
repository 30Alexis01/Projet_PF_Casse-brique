module Color = struct
  type t = Red | Green | Blue | Black | White | RGB of (float * float * float)
  let red = Red
  let green = Green
  let blue = Blue
  let black = Black
  let white = White
  let rgb r g b = RGB (r, g, b)
end