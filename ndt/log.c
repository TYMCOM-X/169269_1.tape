
float l2( x ) float x; {
		float res, mp;
		#define resolution = epsilon(res)  /* or whatever */
	if (x <= 0) error;
	res = read_exponent( x );
	x = fraction( x );
	while (x >= 2) {res++; x /= 2}
	while (x < 1) {res--; x *= 2}
	if (x != 1.0)
	    for (mp=0.5; mp > resolution; mp /= 2)
		if (2 <= (x*=x)) { res += mp; if ((x/=2)==1.0) break; }
	}

-----
Alg C (4.3.3 p 283)

bit *n
    