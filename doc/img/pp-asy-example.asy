import settings; libgs="";
import geometry;
size(7.5cm,0);

// Affichage du repère par défaut (O,vec{i},vec_{j})
show(defaultcoordsys);

real a=5, b=4, theta=-27, poids=3;
ellipse el = ellipse(origin, a, b);
arc     ar = arc(el,(0,-b),(a,0),CCW);
path p = (0,-b-1)--ar--(a+1,0)--(a+1,-b-1)--cycle;
point pO = (0,0), pM=angpoint(ar,90+theta);
abscissa abscM = nodabscissa(el,pM);
real     timeM = abscM.x;
vector utangM = -dir(el,timeM), 
    unormM = rotate(90)*utangM,
    vpoids=(0,-poids),
    vreactionN = -dot(vpoids,unormM)*unormM,
    vfrottement = -dot(vpoids,utangM)*utangM;

filldraw(p,lightgray,blue);
draw(pO--pM,dashed);
markangle("$\theta$",1.5cm,pM,origin,(1,0));

// Affichage d'un nouveau repère (M,vec{u_{\theta}},vec_{u_{r}})
coordsys R=cartesiansystem(pM,i=utangM,j=unormM);
show("$M$", "$\vec{u_{\theta}}$", "$\vec{u_{r}}$", R, xpen=invisible);

// Affichage des trois vecteurs dans le repère R
point RpM=changecoordsys(R, pM);
show(Label("$\vec{f}$",EndPoint),RpM+vfrottement);
show(Label("$\vec{R}$",EndPoint),RpM+vreactionN);
show(Label("$\vec{P}$",EndPoint),RpM+vpoids);


