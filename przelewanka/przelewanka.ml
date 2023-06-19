let przelewanka input_array =
    (* lista wejściowa z usuniętymi szklankami o zerowej pojemności*)
    let glass =
        let input_list = Array.to_list input_array in
        let filter = List.filter
            (fun (x, _) -> not(x = 0)) input_list in
        Array.of_list filter
    in
    let length = Array.length glass in
    let rec nwd a b =
        if a = 0 then b
        else nwd (b mod a) a
    in
    (* NWD pojemności szklanek *)
    let nwd_capacity =
        Array.fold_left (fun a (x, _) -> nwd (min x a) (max x a)) 0 glass in
    (* stan którego szukamy *)
    let desired_state = Array.init length (fun i -> snd glass.(i)) in
    let test1 = Array.for_all (fun (_, y) -> y mod nwd_capacity = 0) glass in
    let test2 = Array.exists (fun (x, y) -> (x = y || y = 0)) glass in
    let hash_table = Hashtbl.create 1000003 in
    (* liczba ruchów potrzebna do uzyskania danego stanu *)
    let moves = ref (-1) in
    (* kolejka stanów, które możemy uzyskać *)
    let q = Queue.create () in
    (* funkcja sprawdzająca czy wynik znajduje się już w tablicy hashującej *)
    let check_result () =
        if Hashtbl.mem hash_table desired_state then
        begin
            Queue.clear q;
            true
        end
        else false
    in
    (* funkcja dodająca dany stan do tablicy hashującej pod warunkiem, że *)
    (* nie został on już wcześniej dodany                                 *)
    let hash_update new_state =
        if not(Hashtbl.mem hash_table new_state) then
        begin
            Hashtbl.add hash_table new_state (!moves + 1);
            Queue.push new_state q
        end
    in
    (* funkcja przyjmująca pewien stan i indeks, dla którego wykonuje ruchy *)
    (* nalania i wylania wody ze szklanki                                   *)
    let fill_empty state i =
        let state_fill = Array.copy state in
        let state_empty = Array.copy state in
        if not(state.(i) = fst glass.(i)) then
        begin
            state_fill.(i) <- fst glass.(i);
            hash_update state_fill
        end;
        if not(state.(i) = 0) then
        begin
            state_empty.(i) <- 0;
            hash_update state_empty
        end
    in
    (* funkcja przyjmująca pewien stan i indeks, dla którego wykonuje ruch  *)
    (* przelania wody ze szklanki "i" do szklanki "j"                       *)
    let pour state i j =
        let state_copy = Array.copy state in
        if state.(j) + state.(i) <= fst glass.(j) then
        begin
            state_copy.(i) <- 0;
            state_copy.(j) <- state.(j) + state.(i);
            hash_update state_copy
        end
        else
        begin
            state_copy.(i) <- state.(i) - (fst glass.(j) - state.(j));
            state_copy.(j) <- fst glass.(j);
            hash_update state_copy
        end
    in
    if test1 && test2 then
    begin
        (* dodaję stan zerowy do tablicy stanów *)
        hash_update (Array.make length 0);
        while not(Queue.is_empty q) do
            (* aktualnie rozpatrywany stan *)
            let state = Queue.pop q in
            if not(check_result ()) then
            begin
                moves := Hashtbl.find hash_table state;
                (* wykonanie wszystkich możliwych ruchów dla danego stanu *)
                for i = 0 to (length - 1) do
                    fill_empty state i;
                    for j = 0 to (length - 1) do
                    if not(i = j) && not(state.(i) = 0) then pour state i j
                    done
                done
            end
        done;
        if Hashtbl.mem hash_table desired_state then
            Hashtbl.find hash_table desired_state
        else -1
    end
    else if glass = [||] then 0
        else -1;;
