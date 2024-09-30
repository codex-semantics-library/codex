struct snode {
  struct snode? spe_left;
  struct snode? spe_right;
  int i;
};

struct stree {
  struct snode+ sph_root;
};

void main_d(struct stree+);
