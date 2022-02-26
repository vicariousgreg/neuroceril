            # Construct causal knowledge base
            (var recipes (list

                (new Recipe
                    '(DET)
                    '(((the))))

                (new Recipe
                    '(N)
                    '(((old))))

                (new Recipe
                    '(ADJ)
                    '(((old))))

                (new Recipe
                    '(N)
                    '(((man))))

                (new Recipe
                    '(V)
                    '(((man))))

                (new Recipe
                    '(N)
                    '(((boat))))


                # NP -> DET N
                (new Recipe
                    '(NP)
                    '(((DET)) ((N))))

                # NP -> DET ADJ N
                (new Recipe
                    '(NP)
                    '(((DET)) ((ADJ)) ((N))))

                # VP -> V NP
                (new Recipe
                    '(VP)
                    '(((V)) ((NP))))

                # S -> NP VP
                (new Recipe
                    '(S)
                    '(((NP)) ((VP))))


#                # S -> NP VP
#                (new Recipe
#                    '(S)
#                    '(((NP)) ((VP))))
#
#                # NP -> N
#                (new Recipe
#                    '(NP)
#                    '(((N))))
#
#                # NP -> DET N
#                (new Recipe
#                    '(NP)
#                    '(((DET)) ((N))))
#
#                # NP -> NP who VP
#                (new Recipe
#                    '(NP)
#                    '(((NP)) ((who)) ((VP))))
#
#                # VP -> V
#                (new Recipe
#                    '(VP)
#                    '(((V))))
#
#                # VP -> V NP
#                (new Recipe
#                    '(VP)
#                    '(((V)) ((NP))))
#
#                # N -> man
#                (new Recipe
#                    '(N)
#                    '(((man))))
#
#                # N -> tunes
#                (new Recipe
#                    '(N)
#                    '(((tunes))))
#                # N -> pianos
#                (new Recipe
#                    '(N)
#                    '(((pianos))))
#
#                # V -> whistles
#                (new Recipe
#                    '(V)
#                    '(((whistles))))
#                # V -> tunes
#                (new Recipe
#                    '(V)
#                    '(((tunes))))
#
#                # DET -> the
#                (new Recipe
#                    '(DET)
#                    '(((the))))
#
#                #################################################
#
#                # S -> NP VP
#                # NP -> DET N
#
#                # NP -> N VP
#                (new Recipe
#                    '(NP)
#                    '(((N)) ((VP))))
#
#                # VP -> VP PP
#                (new Recipe
#                    '(VP)
#                    '(((VP)) ((PP))))
#
#                # PP -> P NP
#                (new Recipe
#                    '(PP)
#                    '(((P)) ((NP))))
#
#                # DET -> the
#
#                # N -> horse
#                (new Recipe
#                    '(N)
#                    '(((horse))))
#
#                # V -> raced
#                (new Recipe
#                    '(V)
#                    '(((raced))))
#
#                # P -> past
#                (new Recipe
#                    '(P)
#                    '(((past))))
#
#                # N -> barn
#                (new Recipe
#                    '(N)
#                    '(((barn))))
#
#                # V -> fell
#                (new Recipe
#                    '(V)
#                    '(((fell))))

            ))
