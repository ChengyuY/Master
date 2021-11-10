// On considere que le labyrinthe est un systeme de coordonnee 
// Du coup on a l'entree est (5,1) et la sortie est (1,5)

active proctype labyrinthe(){
    Entree:
        if
            :: true -> printf("Enter in (5,1)"); goto 5_1
        fi
    5_1:
        if
            ::true -> printf("Go up to (5,2)"); goto 5_2
        fi
    5_2:
        if
            ::true -> printf("Go left to (4,2)"); goto 4_2
        fi
    4_2:
        if
            ::true -> printf("Go left to (3,2)"); goto 3_2
        fi
    3_2:
        if
            ::true -> printf("Go left to (2,2)"); goto 2_2
        fi
    2_2:
        if
            ::true -> printf("Go up to (2,3)"); goto 2_3
        fi
    2_3:
        if
            ::true -> printf("Go up to (2,4)"); goto 2_4
        fi
    2_4:
        if
            ::true -> printf("Go right to (3,4)"); goto 3_4
        fi
    3_4:
        if
            ::true -> printf("Go right to (4,4)"); goto 4_4
        fi
    4_4:
        if
            ::true -> printf("Go up to (4,5)"); goto 4_5
        fi
    4_5:
        if
            ::true -> printf("Go left to (3,5)"); goto 3_5
        fi
    3_5:
        if  
            ::true -> printf("Go left to (2,5)"); goto 2_5
        fi
    2_5:
        if
            ::true -> printf("Go left to (1,5)"); goto Sortie
        fi
    Sortie:
        printf("Exit!"); assert true       
}