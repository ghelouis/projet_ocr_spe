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

  end



class network =
  object (self)

    val mutable neurons = [||]

    initializer
      neurons <- Array.init 122 (fun i -> new neuron (char_of_int i))

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
  end
