# lavaan.model.graph

This takes a model statement of a lavaan model in R and renders it in dot code (part of graphviz), uses dot to compile it to a jpg and used the jpeg package to display it on the active R display device. It also leave a copy in the working directory.

### Prerequisites

This function requires that graphviz (http://www.graphviz.org) is installed on your machine and has been added to the PATH

### Description

creates and displays a directed graph from lavaan model statement

### Usage

lavaan.model.graph(model,outfile="outfile",out.format=NULL,dpi=300,dot.code=NULL,categorical=NULL)

model	   	a lavaan model statement

outfile    	a string of the name for the output (the .dot file, the .jpg and other optional formats)

out.format 	a string indicating optional addition format to have graph rendered. options 		
	   	include('pdf','png','tiff'...see http://www.graphviz.org/doc/info/output.html for full list.

dpi	   	dpi of the jpg output

dot.code   	optional lines of dot code to be added to top of output dot file

categorical 	optional list of categorical variables and their levels in form 	
		list(c1=c('l1','l2',...),c2=c('l3','l4'))

### Value

a .dot file containing the description of the lavaan model in the dot language and a rendered jpg of the model

#### Examples
mod='\s
x3=~1*x2+x12\s
y=\~x5+x1\s
y\~x3+x12\s
x12~~NA\*x1\s
'

lavaan.model.graph(mod)

mod='
y1\~x0+x1+x2+z\s
z\~x0+x1+x2\s
'

lavaan.model.graph(mod, categorical=list(x=('x0','x1','x2')))


## Authors

* **Donald R. Schoolmaster Jr** - *Initial work* 

