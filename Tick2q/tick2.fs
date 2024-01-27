
module Tick2
open System

//---------------------------Tick2 PartA skeleton code-------------------------------//


module PartACase1 =
    type MEngBoundariesType={
        First:int;
        UpperSecond:int;
        LowerSecond:int;
        Fail:int
    }
    let ( MEngBoundaries:MEngBoundariesType)={  
        First=70;
        UpperSecond=60;
        LowerSecond=50;
        Fail=0}
    type BEngBoundariesType={
        First:int;
        UpperSecond:int;
        LowerSecond:int;
        Third:int;
        Fail:int
    }
    let (BEngBoundaries:BEngBoundariesType)={  
        First=70;
        UpperSecond=60;
        LowerSecond=50;
        Third=40;
        Fail=0;
        
        }
    type MScBoundariesType={
        Distinction:int;
        Merit:int;
        Pass:int;
        Fail:int
    }
    let (MscBoundaries:MScBoundariesType)={
        Distinction=70;
        Merit=60;
        Pass=40;
        Fail=0
    }
    // dummy value to make submodule non-empty
    // Three record types, one data value of each type. Choose suitable names.

module PartACase2 =
    type BoundariesType={
        bound70:Option<string> ;
        bound60:Option<string>;
        bound50:Option<string>;
        bound40:Option<string>;
        bound0:Option<string>;
    }
    let (MScBoundaries:BoundariesType)={
        bound70=Some "Distinction";
        bound60=Some "Merit";
        bound50=Some "Pass";
        bound40=None;
        bound0=Some "Fail"
        
    }
    let (MEngBoundaries:BoundariesType)={
        bound70=Some "First";
        bound60=Some "UpperSecond";
        bound50=Some "LowerSecond";
        bound40=None;
        bound0=Some "Fail"
    }
    let (BEngBoundaries:BoundariesType)={
        bound70=Some "First";
        bound60=Some "UpperSecond";
        bound50=Some "LowerSecond";
        bound40=Some "Third";
        bound0=Some "Fail"
    }


    // dummy value to make submodule non-empty
    // One record type, three data values of this type. Choose suitable names.

module PartACase3 =
    let MEngBoundaries = ["First",70.; "UpperSecond", 60.; "LowerSecond",50.; "Fail",0.]
    let BEngBoundaries = ["First",70.; "UpperSecond", 60.; "LowerSecond",50.; "Third", 40. ;"Fail",0]
    let MScBoundaries = ["Distinction",70.; "Merit", 60.; "Pass",50.; "Fail",0.]
    // dummy value to make submodule non-empty
    // One type, three data values of this type. Choose suitable names.

//---------------------------Tick2 PartB case 2 skeleton code-------------------------------//

module PartBCase2 =

    open PartACase2 // get unqualified access to Case 2 types and values

    /// Return as a Ok string the name of the correct classification for a student
    /// on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0). 
    /// The error message should say what the problem in the data was.
    let classify (course: string) (mark: float) : Result<string,string> =
        let boundaryRes=
            match course with
            | "MSc" -> Ok(MScBoundaries)
            | "MEng" -> Ok(MEngBoundaries)
            | "BEng" -> Ok(BEngBoundaries)
            |   _ -> Error (sprintf "no %s course" course)

        let (Empty:BoundariesType)={
            bound70=None;
            bound60=None;
            bound50=None;
            bound40=None;
            bound0=None
            }
        let boundary=Result.defaultValue Empty boundaryRes

        let result=
            match mark with
            | _ when (mark<=100.) && (mark>=70.) -> Ok (boundary.bound70)
            | _ when (mark<70.) && (mark>=60.) -> Ok (boundary.bound60)
            | _ when (mark<60.) && (mark>=50.) -> Ok (boundary.bound50)
            | _ when (mark<50.) && (mark>=40.) -> Ok (boundary.bound40)
            | _ when (mark<40.) && (mark>=0.) -> Ok (boundary.bound0)
            | _ -> Error (sprintf "mark %f is out of range" mark)
        let convertOptionToString opt = 
            match opt with
            | Some str -> str
            | None -> ""
        match result with
            | Ok None -> Ok (convertOptionToString(boundary.bound0))
            | Ok sth -> Ok(convertOptionToString(sth))
            | _ -> Error ("Unexpected Error")
        
        
        
        


//---------------------------Tick2 PartB case 3 skeleton code-------------------------------//

module PartBCase3 =

    open PartACase3 // get unqualified access to Case 3 types and values

    /// Return as a Ok string the name of the correct classification for a studen on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0). The error message should say what the problem in the data was.
    let classify (course: string) (mark: float) : Result<string,string> =
        let boundaryRes=
            match course with
            | "MSc" -> Ok(MScBoundaries)
            | "MEng" -> Ok(MEngBoundaries)
            | "BEng" -> Ok(BEngBoundaries)
            |   _ -> Error (sprintf "no %s course" course)
        let boundary=Result.defaultValue (["1",0.]) boundaryRes
        let boundOption=
            boundary  
            |> List.tryFind (fun (str, num) -> num < mark)
            |> Option.map fst

        match boundOption with
        | _ when (mark >100) || (mark<0)-> Error((sprintf "mark %f is out of range" mark))
        | Some str ->Ok( str)
        | _ -> Error("Unexpected")
        

        //the advantage of this approach is that you can easily change the mark boundaries 
        //as we are looking for the first boundary that is smaller than the mark
        //whereas in case2 the boundaries are fixed in the names of the records and we have to find the boundries through match
        

//------------------------------------Tick2 PartC skeleton code-----------------------------------//

module PartC =
    open PartACase3 // get unqualified access to Case 3 types and values
    open PartBCase3 // get unqualified access to classify function

    type Marks = {Mark1: float} // simplified set of marks (just one mark) used for compilation of code

    /// Return the total mark for a student used to determine classification. 
    /// marks:  constituent marks of student on given course.
    /// course: name of course student is on
    /// Return None if the course is not valid or any of the marks are
    /// outside the correct range 0 - 100.
    let markTotal (marks: Marks) (course: string) : float option =
        match course with
        | "MEng"  | "BEng" | "MSc" when marks.Mark1 <= 100.0 && marks.Mark1 >= 0.0 ->
            Some marks.Mark1 // in this case with only one mark, student total is just the mark!
        | _ -> None
        

    /// Operation:
    /// 1. Return an error if boundary is not a valid boundary for course.
    /// 2. Return IsAboveBoundary = true if total is above or equal to boundary
    /// 3. Return Uplift = Some uplift if total is in the valid possible uplift range (0 - -2.5%) of boundary.
    let upliftFunc 
        (marks: Marks) 
        (boundary:string) 
        (course: string)
            : Result<{|IsAboveBoundary: bool; Uplift:float option|}, string> =
        
        failwithf "Not Implemented" // do not change - implementation not required
         /// Operation:
    /// 1. Return an error if boundary is not a valid boundary for course.
    /// 2. Return IsAboveBoundary = true if total is above or equal to boundary
    /// 3. Return Uplift = Some uplift if total is in the valid possible uplift range (0 - -2.5%) of boundary.

    /// Given a list of boundaries, and a course, and a student's marks:
    /// Return the student classification, or an error message if there is
    /// any error in the data.
    /// boundaries: name only, subfunctions will know boundary marks based on course, 
    /// this function needs only the results of calling its subfunctions.
    let classifyAndUplift 
        (boundaries: string list)
        (course: string) 
        (marks: Marks)
                : Result<string,string> =
        let totalMarks=
            match (markTotal marks course) with
            | Some f -> f
            | None -> 0.
        

        let checkBoundary (result : Result<{|IsAboveBoundary: bool; Uplift:float option|}, string>) =
            match result with
            | Ok record when(record.Uplift=None) -> false
            | Ok record when(record.Uplift<>None) -> true
            | Ok _ -> false
            | Error _ -> false //if there are uplift available, then we are sure that that boundary can be reached, therefore we return true

        let UpliftBoundary= List.tryFind(fun str ->  (checkBoundary (upliftFunc marks str course)) ) boundaries
        //try finds the string in the boundaries list with a uplift availble, since there are only one, we can only find one.

        match UpliftBoundary with
            | None -> (classify course totalMarks)
            | Some string -> Ok (string)
            | _ -> Error ("unknown error")


        // Use upliftFunc and markTotal and classify.
        // Assume that the student can be within possible uplift range of at most one boundary.
        // Assume that classify is correct unless student is within uplift range of a given boundary,
        // If student is within uplift range of a boundary `boundaryName` work out classification as:
            // let total = markTotal marks course
            // let effectiveMark = total + upliftFunc marks boundaryName course
            // let className = classify course effectiveMark
            // Return Ok classname or an error if there is any error.
            // (option and error returns ignored in above comments, must be dealt with)

        // failwithf "Not implemented" // replace by your code ()

//------------------------------Simple test data and functions---------------------------------//
module TestClassify =
    /// test data comaptible with the Tick 2 problem
    let classifyUnitTests = [
        "MEng",75.0, Ok "First"
        "MSc", 75.0,Ok "Distinction"
        "BEng", 75.0, Ok "First"
        "MEng",65.0, Ok "UpperSecond"
        "MSc", 65.0, Ok "Merit"
        "BEng", 65.0, Ok "UpperSecond"        
        "MEng",55.0, Ok "LowerSecond"
        "MSc", 55.0, Ok "Pass"
        "BEng", 55.0, Ok "LowerSecond"        
        "MEng",45.0, Ok "Fail"
        "MSc", 45.0, Ok "Fail"
        "BEng", 45.0, Ok "Third"
        "BEng", 35.0, Ok "Fail"        
    ]

    let runClassifyTests unitTests classify testName =
        unitTests
        |> List.map (fun (data as (course,mark,_)) -> classify course mark, data)
        |> List.filter (fun (actualClass, (_,_,className)) -> actualClass <> className)
        |> function 
            | [] -> printfn $"all '{testName}' tests passed."
            | fails -> 
                fails 
                |> List.iter (fun (actual, (course,mark,className)) 
                                -> printfn $"Test Failed: {course}, {mark}, expected className={className}, \
                                    actual className={actual}")


//-------------------------------------------------------------------------------------------//
//---------------------------------Run Part B tests------------------------------------------//
//-------------------------------------------------------------------------------------------//

open TestClassify
let runTests() =
    runClassifyTests classifyUnitTests PartBCase2.classify "Case2"
    runClassifyTests classifyUnitTests PartBCase3.classify "Case3"


//-------------------------------------------------------------------------------------------//
//---------------------------------Tick2 Part X Skeleton code--------------------------------//
//-------------------------------------------------------------------------------------------//
module PartX =
    type Lens<'A,'B> = ('A -> 'B) * ('B -> 'A -> 'A)

    let lensMap (lens: Lens<'A,'B>) (f: 'B -> 'B) (a: 'A) =       
        (fst lens a |> f |> snd lens) a

    let mapCAndB (lensC: Lens<'A,'C>) (lensB: Lens<'A,'B>) (fc:'C->'C) (fb: 'B->'B) =
        lensMap lensC fc >> lensMap lensB fb

    let combineLens (l1: Lens<'A,'B>) (l2: Lens<'B,'C>) : Lens<'A,'C> =
        let ab= fst l1 
        let bc= fst l2
        let baa = snd l1
        let cbb = snd l2
        
        (fun x -> bc(ab x)), (fun x  z->  baa (cbb x (ab z)) z)  
        



