{-
- Barrientos Alvarez Jorge Miguel Aaron  |  jma.barrientos@ciencias.unam.mx
-}


module FormasNormales where

import LogPro

--Función que dada una fórmula f, regresa una fórmula equivalente a f en su Forma Normal Negativa
formaNormalNegativa:: Prop->Prop
formaNormalNegativa (Var x)=(Var x)
formaNormalNegativa x = auxNegativa(equivalencia(x))


--Función que dada una fórmula f, regresa una fórmula equivalente a f en su Forma Normal Conjuntiva
formaNormalConjuntiva :: Prop -> Prop
formaNormalConjuntiva (Conj(Disy p q) r) = formaNormalConjuntiva
                                            (Disy
                                             (Conj (formaNormalConjuntiva p) (formaNormalConjuntiva r))
                                             (Conj (formaNormalConjuntiva q) (formaNormalConjuntiva r)))
formaNormalConjuntiva (Conj p (Disy q r) ) = formaNormalConjuntiva
                                            (Disy
                                             (Conj (formaNormalConjuntiva p) (formaNormalConjuntiva q))
                                             (Conj (formaNormalConjuntiva p) (formaNormalConjuntiva r)))
formaNormalConjuntiva (Disy x y) = Disy(formaNormalConjuntiva x)(formaNormalConjuntiva y)
formaNormalConjuntiva p = p




--Función que dada una fórmula f, regresa una fórmula equivalente a f en su Forma Normal Disyuntiva
formaNormalDisyuntiva:: Prop->Prop
formaNormalDisyuntiva (Disy(Conj p q) r) = formaNormalDisyuntiva
                                            (Conj
                                             (Disy (formaNormalDisyuntiva p) (formaNormalDisyuntiva r))
                                             (Disy (formaNormalDisyuntiva q) (formaNormalDisyuntiva r)))
formaNormalDisyuntiva (Disy p (Conj q r) ) = formaNormalDisyuntiva
                                            (Conj
                                             (Disy (formaNormalDisyuntiva p) (formaNormalDisyuntiva q))
                                             (Disy (formaNormalDisyuntiva p) (formaNormalDisyuntiva r)))
formaNormalDisyuntiva (Conj x y) = Conj(formaNormalDisyuntiva x)(formaNormalDisyuntiva y)
formaNormalDisyuntiva p = p
                                             

                                      

----------------------------------------------------------------------------------
-----------------------------------AUXILIARES-------------------------------------
----------------------------------------------------------------------------------
--Negativa
auxNegativa::Prop->Prop
auxNegativa (Var x)=(Var x)
auxNegativa (Neg(Var x))=Neg(Var x)
auxNegativa (Neg(Conj x y))=negacion(Conj x y)
auxNegativa (Neg(Disy x y))=negacion(Disy x y)
auxNegativa (Conj x y)=Conj(auxNegativa x)(auxNegativa y)
auxNegativa (Disy x y)=Disy(auxNegativa x)(auxNegativa y)

