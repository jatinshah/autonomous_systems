{-
		QuadTree Generation
		Name: Jatin Shah
	
	Function quadTree creates quadTree for each polygon.
	Function merge merges those quadTrees
	
		
-}
module QuadTree where

type Rectangle = [Vertex]
type Vertex = (Float,Float)
data QuadTree =     Empty Rectangle
	 	 |  Full Rectangle
		 |  Branch QuadTree QuadTree QuadTree QuadTree 
		 deriving Show

-- Do two lines intersect? 
-- Segments one same line overlapping considered as intersection
intrsct line1 line2 = let 
			 (x0,y0) = fst line1
			 (x1,y1) = snd line1
			 (p0,q0) = fst line2
			 (p1,q1) = snd line2
			 a1=y1-y0;a2=x1-x0
			 b1=q1-q0;b2=p1-p0
			 x=b2*a2*(y0-q0)-a1*b2*x0+a2*b1*p0
			 xmin=min x0 x1;xmax=max x0 x1
			 pmin=min p0 p1;pmax=max p0 p1
			 m=b1*a2-a1*b2
			in
			a1*b2/=b1*a2 && xmin<x/m && x/m<xmax && pmin<x/m && x/m<pmax

-- Does a line intersect a polygon?
intersectp [p] line = False
intersectp (p1:p2:polygon) line = let
					linep = (p1,p2)
				  in
				  	intrsct line linep || intersectp (p2:polygon) line



isLeftOf (px,py) ((ax,ay),(bx,by))  = let (s,t) = (px-ax,py-ay)
				          (u,v) = (px-bx,py-by)
				        in (s*v) >= (t*u)
-- Containment relation
-- operator >= : inclusion with boundary considered
contains polygon point = let isLeftOfp p' = isLeftOf point p'
		             leftOfList   = map isLeftOfp (zip polygon (tail polygon ++ [head polygon]))
		       	    in  and leftOfList

	
-- Whether polygon2 is fully contained in at least one of polygonList.
-- Note: Order is important in this case
fullOverlap polygon1 polygon2 = and (map (contains polygon1) polygon2)
noOverlap polygon1 polygon2 = all (\x->x==False) (map (contains polygon1) polygon2) 

-- Check if diagonals of polygon2 intersect polygon1 at non-endpoints of either segments
-- Only case in which this will fail to catch overlap is when polygon1 is completely inside polygon2.
-- but this case is considered when this function is used.
partialOverlap polygon1 polygon2 = let diag=[(x,y) | x<-polygon2, y <- polygon2] -- Includes edges but they do not affect the computation
				       intersections = map (intersectp $ polygon1++[head polygon1]) diag
				    in or intersections

-- Find Boundaries of the region
-- Returns a rectangle containing all the polygons
findBoundary polygonList = let
				(xCoord,yCoord)= unzip (concat polygonList)
				xmin = minimum xCoord
				xmax = maximum xCoord
				ymin = minimum yCoord
				ymax = maximum yCoord
			   in 
			   	[(xmin,ymin),(xmax,ymin),(xmax,ymax),(xmin,ymax)]

quadTree :: [Vertex] -> Integer -> [Vertex] -> QuadTree
quadTree region 0 polygon = if noOverlap polygon region && noOverlap region polygon 
				then Empty region
				else Full region
quadTree region level polygon = let
			 		[point1,point2,point3,point4]=region
					mid1=((fst point1+fst point2)/2,(snd point1+snd point2)/2)
					mid2=((fst point2+fst point3)/2,(snd point2+snd point3)/2)
					mid3=((fst point3+fst point4)/2,(snd point3+snd point4)/2)
					mid4=((fst point4+fst point1)/2,(snd point4+snd point1)/2)
					midd=((fst point1+fst point3)/2,(snd point1+snd point3)/2)
					quad1=[point1,mid1,midd,mid4]
					quad2=[mid1,point2,mid2,midd]
					quad3=[midd,mid2,point3,mid3]
					quad4=[mid4,midd,mid3,point4]
				  in
				  	-- Region fully inside polygon
					if fullOverlap polygon region 
						then Full region
					-- region and polygon disjoint
						else if noOverlap polygon region && noOverlap region polygon
						        then Empty region
					-- region and polygon Overlap
					-- Case 1: Diagonals of the region intersect polygon at
					-- points which are not end-points of diagonals
					-- Case 2: polygon fully inside region
						        else if partialOverlap polygon region || fullOverlap region polygon
						     	        then Branch (quadTree quad1 (level-1) polygon)(quadTree quad2 (level-1) polygon)(quadTree quad3 (level-1) polygon)(quadTree quad4 (level-1) polygon)
							        else Empty region

-- Merge two quadtrees
-- Though it might be possible to consider all the polygons at the same time in the quadTree function,
-- I thought this approach might be more interesting as it explores some kind of algebra of quadTrees
merge quadTree1 quadTree2 = case (quadTree1,quadTree2) of
				 (Empty _,_)-> quadTree2
				 (_,Full _) -> quadTree2
				 (Full _,_) -> quadTree1
				 (_,Empty _) -> quadTree1
				 (Branch p1 p2 p3 p4,Branch q1 q2 q3 q4) 
					-> case (t1,t2,t3,t4) of
						(Empty r1,Empty r2,Empty r3,Empty r4)
							-> Empty [r1 !! 0, r2 !! 1,r3 !! 2, r4 !! 3]
						(Full r1,Full r2,Full r3,Full r4)
							-> Full [r1 !! 0, r2 !! 1, r3 !! 2, r4 !! 3]
						_
							-> Branch t1 t2 t3 t4
					   where
					   	t1=merge p1 q1
						t2=merge p2 q2
						t3=merge p3 q3
						t4=merge p4 q4
				

main polygonList level = let
				region = findBoundary polygonList
				quadTreelist = map (quadTree region level) polygonList
			 in
			   	foldr merge (Empty region) quadTreelist

-- End of Module
