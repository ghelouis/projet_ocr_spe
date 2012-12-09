let img2matbool img =
  let xl = (Sdlvideo.surface_info img).Sdlvideo.w and
      yl = (Sdlvideo.surface_info img).Sdlvideo.h in
  let mat = Array.make_matrix 30 30 1 in
  for i = 0 to 29 do
    for j = 0 to 29 do
      let x = int_of_float((float_of_int(i*xl)) /. 30.) in
      let y = int_of_float((float_of_int(j*yl)) /. 30.) in
      if ((0,0,0) = Sdlvideo.get_pixel_color img x y)
      then
        mat.(i).(j) <- 0
    done
  done;
  mat
      
let file_names =
  let lines = ref [] in
  let chan = open_in "../apprentissage/a.txt" in
  try
    while true; do
      lines := input_line chan::!lines
    done; [||]
  with End_of_file ->
    close_in chan;
    Types.list2tab !lines


class neuron c =
  object (self)
    val name:char = c
    val mutable stats_m = [||]
  
    initializer
      stats_m <- Array.make_matrix 30 30 50

    method get_name = name

    method compare mat =
      let izgoud = ref 0 in
      for i = 0 to 29 do
        for j = 0 to 29 do
          if (mat.(i).(j) = 0)
          then
            izgoud := (!izgoud) + 2 * stats_m.(i).(j) - 100
          else
            izgoud := (!izgoud) - 2 * stats_m.(i).(j) + 100
        done
      done;
      !izgoud

    method learn imgs =
      let stats_m = Array.make_matrix 30 30 0 in
      let l = Array.length imgs in
      let bin_imgs = Array.init l (fun i -> img2matbool imgs.(i-1)) in
      for i = 0 to 29 do
        for j = 0 to 29 do
          for k = 0 to l do
            stats_m.(i).(j) <- stats_m.(i).(j) + bin_imgs.(k).(i).(j)
          done;
          stats_m.(i).(j) <- 100 * stats_m.(i).(j) / l
        done
      done

  end



class network =
  object (self)

    val mutable neurons = [||]

    initializer
      neurons <- Array.init 91 (fun i -> new neuron (char_of_int (i+21)))

    method find_char c =
      let mat_c = Array.make_matrix 30 30 0 in
      for i = 0 to 29 do
        for j = 0 to 29 do
          let x = (int_of_float)((float_of_int
              (i*Array.length c.Types.tab)) /. 30.) in
          let y = (int_of_float)((float_of_int
              (j*Array.length c.Types.tab.(0))) /. 30.) in
          mat_c.(i).(j) <- c.Types.tab.(x).(y)
        done
      done;
      let finded_c = ref ' ' and best_r = ref (-10000) in
      for i = 0 to 221 do
        let result = neurons.(i)#compare mat_c in
        if (result > !best_r)
        then
          finded_c := neurons.(i)#get_name;
          best_r := result
      done;
      !finded_c

    method learn =
      for c = 0 to 90 do
        let imgs = Array.init 26 (fun _ -> 
            Sdlloader.load_image "../apprentissage/z-Verdana.ttf.bmp") in
        for i = 0 to 25 do 
          imgs.(i) <- Sdlloader.load_image  
              ("../apprentissage/"^(string_of_int c)^(file_names.(i)))
        done;
        neurons.(c)#learn imgs
      done
  end

(*to_channel*)
let serialization  my_object = 
let out_channel = open_out_bin "my_binary" in
Marshal.to_channel out_channel my_object [Marshal.Closures];
close_out out_channel;;

(*from_channel*) 
let deserialization =
  let in_channel = open_in_bin "my_binary" in
  let data = Marshal.from_channel in_channel in 
  data;;



(*let marshall2 my_binobject = Marshall.from_string "my_binobject" 0 ;;*)














