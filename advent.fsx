let i n=System.IO.File.ReadAllText(sprintf "%s/input/input%d.txt"__SOURCE_DIRECTORY__ n)


// Day #1 (a)
i 1|>Seq.sumBy(" (".IndexOf)

// Day #1 (b)
i 1|>Seq.map(" (".IndexOf)|>Seq.scan(+)0|>Seq.findIndex((=)(-1))



// Day #2 (a)
(i 2).Split '\n'|>Seq.sumBy(fun s->(fun(|I|)(-)->let[|I l;I w;I h|]=s.Split 'x'in 2*(l*w+w*h+h*l)+(l*w-w*h-h*l))int min)

// Day #2 (b)
(i 2).Split '\n'|>Seq.sumBy(fun s->(fun(|I|)(/)->let[|I l;I w;I h|]=s.Split 'x'in 2*(l+w+h-l/w/h)+w*h*l)int max)



// Day #3 (a)
i 3|>Seq.scan(fun(x,y)c->let i="^ v< >".IndexOf c in x+i/3*(i-4),y+(i%2-1)*(i-1))(0,0)|>Seq.countBy id|>Seq.length

// Day #3 (b)
i 3|>Seq.map"^ v< >".IndexOf|>Seq.scan(fun[x,y;o]i->[o;x+i/3*(i-4),y+(i%2-1)*(i-1)])[0,0;0,0]|>Seq.concat|>Seq.distinct|>Seq.length



// Day #5 (a)
(i 5).Split '\n'|>Seq.filter((+)" ">>Seq.pairwise>>Seq.fold(fun(d,v)(a,b)->d||a=b,if b=a+'\001'&&(Seq.exists((=)a)"pacx")then 1<<<31 else v+sign(1+"aeiou".IndexOf b))(false,0)>>fun(d,v)->v>2&&d)|>Seq.length

// Day #5 (b)
(i 5).Split '\n'|>Seq.filter(fun s->"  "+s|>Seq.windowed 3|>Seq.mapi(fun i[|a;b;c|]->s.IndexOf(string b+string c)<i-2,a=c)|>Seq.reduce(fun(a,b)(c,d)->a||c,b||d)|>fun(a,b)->a&&b)|>Seq.length