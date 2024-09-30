type nat = int with self >= 0
type pos = int with self > 0

/* type vector = ∃ l: (int with self > 0). struct {
  (int with self = l) len;
  int[l] arr;
} */

type vector(sz) = struct {
  (int with self = sz) len ;
  int[sz] arr;
}

type vectorptr =  ∃ l: pos. (vector(l)+)

/* void read(vector+ p, int+ res, nat i); */
/* void read(vectorptr p, int+ res, int i); */
void read(vectorptr p, int+ res, nat i);

void read_wrong(vectorptr p, int+ res, nat i); 

/* void write(vector+ p, nat i, int v); */
void write(vectorptr p, nat i, int v);

/* void iter(vector+ p, int+ res); */
void iter(vectorptr p, int+ res);
