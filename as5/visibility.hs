module VisibilityGraph where

-- Do two lines intersect? 
intrsct line1 line2 = let 
			 (x0,y0) = head line1
			 (x1,y1) = last line1
			 (p0,q0) = head line2
			 (p1,q1) = last line2
			 m1 = (y0-y1)/(x0-x1) -- x0 != x1
			 m2 = (q0-q1)/(p0-p1) -- p0 != p1
			 x = ((y0-q0)-m1*x0+m2*p0)
			in
			(m1/=m2) && ((min x0 x1) <= x/(m2-m1) && x/(m2-m1) <= (max x0 x1)) &&((min p0 p1) <= x/(m2-m1) && x/(m2-m1)<= (max p0 p1))

-- Does a line intersect a polygon?
intersectp line [p] = False
intersectp line (p1:p2:polygon) = let
					linep = [p1,p2]
				  in
				  	intrsct line linep || intersectp line (p2:polygon)

-- Does a line intersect a list of polygons?
intersect line [] = False
intersect line (polygon:rest) = intersectp line polygon || intersect line rest

-- Create the edges 
createEdges polygonList [] point = []
createEdges polygonList (node:nodes) point = 
		let x=if(point==node || intersect [point,node] polygonList) then [] else [node]
		in
		    x++createEdges polygonList nodes point
createGraph polygonList start end = let
					nodes=start:end:concat polygonList
					links=map (createEdges polygonList nodes) nodes
				    in
				    	zip nodes links

-- End of Module
