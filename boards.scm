(define empty-board "
                 ___
             ___/   \\___
         ___/   \\___/   \\___
     ___/   \\___/   \\___/   \\___
 ___/   \\___/   \\___/   \\___/   \\___
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
/   \\___/   \\___/   \\___/   \\___/   \\
\\___/   \\___/   \\___/   \\___/   \\___/
    \\___/   \\___/   \\___/   \\___/
        \\___/   \\___/   \\___/
            \\___/   \\___/
                \\___/")

(define board-with-coords "
                 ___
             ___/0 0\\___
         ___/0 1\\___/1-1\\___
     ___/0 2\\___/1 0\\___/2-2\\___
 ___/0 3\\___/1 1\\___/2-1\\___/3-3\\___
/0 4\\___/1 2\\___/2 0\\___/3-2\\___/4-4\\
\\___/1 3\\___/2 1\\___/3-1\\___/4-3\\___/
/1 4\\___/2 2\\___/3 0\\___/4-2\\___/5-4\\
\\___/2 3\\___/3 1\\___/4-1\\___/5-3\\___/
/2 4\\___/3 2\\___/4 0\\___/5-2\\___/6-4\\
\\___/3 3\\___/4 1\\___/5-1\\___/6-3\\___/
/3 4\\___/4 2\\___/5 0\\___/6-2\\___/7-4\\
\\___/4 3\\___/5 1\\___/6-1\\___/7-3\\___/
/4 4\\___/5 2\\___/6 0\\___/7-2\\___/8-4\\
\\___/5 3\\___/6 1\\___/7-1\\___/8-3\\___/
/5 4\\___/6 2\\___/7 0\\___/8-2\\___/9-4\\
\\___/6 3\\___/7 1\\___/8-1\\___/9-3\\___/
/6 4\\___/7 2\\___/8 0\\___/9-2\\___/A-4\\
\\___/7 3\\___/8 1\\___/9-1\\___/A-3\\___/
    \\___/8 2\\___/9 0\\___/A-2\\___/
        \\___/9 1\\___/A-1\\___/
            \\___/A 0\\___/
                \\___/")

(define board-with-Xs "
                 ___
             ___/   \\___
         ___/   \\_X_/   \\___
     ___/   \\_X_/   \\_X_/   \\___
 ___/   \\_X_/   \\_X_/   \\_X_/   \\___
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
/   \\_X_/   \\_X_/   \\_X_/   \\_X_/   \\
\\_X_/   \\_X_/   \\_X_/   \\_X_/   \\_X_/
    \\_X_/   \\_X_/   \\_X_/   \\_X_/
        \\_X_/   \\_X_/   \\_X_/
            \\_X_/   \\_X_/
                \\_X_/")