module DiamondSquare =

    //create types for defining shapes

    ///Square, defined by its side length
    type Square = Square of int
    ///Diamond, defined by length between to corners
    type Diamond = Diamond of int
    ///Defined by length of the side of a square that the ovject is inscribed in
    type Shape =
        | Square of Square
        | Diamond of Diamond
    
    ///the X and Y position
    type Coordinates = {X: int; Y: int}

    ///The Hieghtmap of a given chunk of region as a series of floats that are the offset from the base hieght
    //was HieghtMap = HieghtMap of float[,], but was changed so that any 2D float array would be accepted
    type HieghtMap = float[,]

    //Create matrix of zeroes of chunk size to initilize this variable
    let matrix = Array2D.zeroCreate<float> 9 9

    //locate center of shape
    //  since each shape is a square, or can be inscribed within one, pass it a matrix and find the
    //  coordinate of the center (same value for i and j)
    ///Finds center of shape inscribed within a square. Takes a matrix, returns coordinates for within the matrix
    let locateCenterpoint (matrixLocal:HieghtMap) = 
        let coord = int ((Array2D.length1 matrixLocal) - 1) / 2
        {X = coord; Y = coord;}
    
    //locate corners of a shape that is inscribed in a square
    ///Returns list of corner values for a given shape. Takes a matrix and returns a list of Coordinates
    let getCorners (shape:string) (matrixLocal:HieghtMap) =
        let farSide = Array2D.length1 matrixLocal - 1
        let getSquareCorners = 
            {X = 0; Y = 0}::{X = farSide; Y = 0}::{X = 0; Y = farSide}::{X = farSide; Y = farSide}::[]
        let getDiamondCorners =
            {X = farSide / 2; Y = 0}::{X = farSide; Y = farSide / 2}::{X = farSide / 2; Y = farSide}::{X = 0; Y = farSide / 2}::[]
        match shape with
        | "Square" -> getSquareCorners
        | _ -> getDiamondCorners
    
    //pulls hieghts for a given list of coordinates
    ///gets hieghts of a given list of coordinates and a matrix and returns a list of the values
    let getHieghts (corners:Coordinates list) (matrixLocal:HieghtMap) = 
        let getHieght (c:Coordinates) =
            let x = c.X
            let y = c.Y
            matrixLocal.[x,y]
        let rec loop acc = function
            | [] -> acc
            | x::xs -> loop ((getHieght x)::acc) xs
        loop [] corners

    //diamond step - takes a square and offsets the centerpoint
    //Takes a matrix and offsets the centerpoint of the square
    let diamondStep (matrixLocal:HieghtMap) = 
        let rand = 3.0
        let center = locateCenterpoint matrixLocal
        let hieghts = getHieghts <| getCorners "Square" <| matrixLocal
        let offset = rand + (List.average hieghts)
        matrixLocal.[center.X, center.Y] <- offset

