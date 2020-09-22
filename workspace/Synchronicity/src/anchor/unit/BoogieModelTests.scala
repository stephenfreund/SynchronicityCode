  package anchor.unit

import org.scalatest.FunSuite
import anchor.boogie._


  class BoogieModelTests extends FunSuite {

    test("Good1") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |x -> 3
          |y -> true
          |z -> (_ (as-array) (k!76))
          |k!89 -> {
          |  T@Test!val!0 -> 8
          |  T@Test!val!11 -> 98
          |  else -> 1
          |}
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good2") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |m -> (lambda (_ (x!1 (x))) true)
          |m -> (lambda (_ (x!1 (x))) true)
          |x -> (lambda (_ (x!1 (T@Test))) (ite (= (x!1) T@Test!val!2) a b))
          |z -> (lambda (_ (x!1 (T@Test))) (ite (= (x!1) T@Test!val!2) (a) (b)))
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good3") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |Test._state -> (lambda (_ (x!1 (T@Test))) (ite (= (x!1) T@Test!val!2) (FRESH) (ite (= (x!1) T@Test!val!3) (FRESH) (ite (= (x!1) T@Test!val!4) (FRESH) (SHARED)))))
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good4") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          | Test._state@0 ->(lambda (_ (x!1 (T@Test))) (let (_ (a!1 (moo))) 3 ))
          |Test._state@0 ->(lambda (_ (x!1 (T@Test))) (let (_ (a!1 (moo))) (ite (= (x!1) T@Test!val!4) (FRESH) (ite (= (x!1) T@Test!val!4) 1 2) )))
          |Test._state@0 ->(lambda (_ (x!1 (T@Test))) (let (_ (a!1 (moo))) (ite (= (x!1) T@Test!val!4) (FRESH) (ite (= (x!1) T@Test!val!4) (FRESH) (SHARED)) )))
          |
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good5") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |Test._state@0 ->(lambda (_ (x!1 (T@Test))) (let (_ (a!1 (moo))) (ite (= (x!1) T@Test!val!4) (FRESH) (ite (= (x!1) T@Test!val!4) (let (_ (a!1 (moo))) (ite (= (x!1) T@Test!val!4) (FRESH) (ite (= (x!1) T@Test!val!4) (FRESH) (SHARED)) )) (SHARED)) )))
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good6") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |Test._mutex@1 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!5) 14 (ite (= (x!1) T@Test!val!7) 8 9)))) (a!1)))
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good7") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |Test._mutex@1 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!5) 14 (ite (= (x!1) T@Test!val!7) 8 (ite (= (x!1) T@Test!val!17) 29 1))))) (a!1)))
          |*** END_MODEL
        """.stripMargin))
    }

    test("Good8") {
      val model = BoogieModel.parse((
        """
          |*** MODEL
          |Test._mutex@1 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!5) 14 (ite (= (x!1) T@Test!val!7) 8 (ite (= (x!1) T@Test!val!17) 29 1)))))
          |					      (let (_ (a!2 (ite (= (x!1) T@Test!val!13) 16 (ite (= (x!1) T@Test!val!6) 15 (ite (= (x!1) T@Test!val!11) 7 (a!1))))))
          |						(let (_ (a!3 (ite (= (x!1) T@Test!val!16) 26 (ite (= (x!1) T@Test!val!0) 5 (ite (= (x!1) T@Test!val!9) 3 (a!2))))))
          |						  (ite (= (x!1) T@Test!val!15) 25 (a!3))))))
          |
          |*** END_MODEL
        """.stripMargin))
    }

    test("Big0") {
      val m =         """*** MODEL
                        |%lbl%@10 -> false
                        |%lbl%@18 -> true
                        |%lbl%+12 -> true
                        |%lbl%+13 -> true
                        |%lbl%+27 -> true
                        |%lbl%+28 -> true
                        |%lbl%+37 -> true
                        |%lbl%+38 -> true
                        |%lbl%+4 -> true
                        |_B -> T@Mover!val!0
                        |_E -> T@Mover!val!4
                        |_L -> T@Mover!val!2
                        |_N -> T@Mover!val!3
                        |_pc@0 -> T@Phase!val!0
                        |_pc@1 -> T@Phase!val!0
                        |_pc@2 -> T@Phase!val!0
                        |_R -> T@Mover!val!1
                        |i@0 -> 10
                        |i@2 -> 19
                        |i@3 -> 20
                        |m@0 -> T@Mutex!val!0
                        |m@1 -> T@Mutex!val!8
                        |MatchingLeft -> T@Phase!val!1
                        |MatchingRight -> T@Phase!val!0
                        |mover166@0 -> T@Mover!val!0
                        |Mutex._mutex -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!54) 109 (ite (= (x!1) T@Mutex!val!55) 112 (ite (= (x!1) T@Mutex!val!53) 106 1))))) (ite (= (x!1) T@Mutex!val!56) 115 (ite (= (x!1) T@Mutex!val!52) 103 (ite (= (x!1) T@Mutex!val!51) 101 (a!1))))))
                        |Mutex._mutex@0 -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!61) 126 (ite (= (x!1) T@Mutex!val!63) 132 (ite (= (x!1) T@Mutex!val!60) 123 1))))) (ite (= (x!1) T@Mutex!val!62) 129 (ite (= (x!1) T@Mutex!val!59) 120 (ite (= (x!1) T@Mutex!val!51) 100 (a!1))))))
                        |Mutex._mutex@2 -> (_ (as-array) (k!77))
                        |Mutex._mutex@3 -> (_ (as-array) (k!85))
                        |Mutex._mutex187 -> (_ (as-array) (k!77))
                        |Mutex._mutex187_post -> (_ (as-array) (k!85))
                        |Mutex._state -> (lambda (_ (x!1 (T@Mutex))) (ite (= (x!1) T@Mutex!val!3) (FRESH) (ite (= (x!1) T@Mutex!val!4) (FRESH) (ite (= (x!1) T@Mutex!val!1) (FRESH) (SHARED)))))
                        |Mutex._state@0 -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!21) (FRESH) (ite (= (x!1) T@Mutex!val!48) (FRESH) (ite (= (x!1) T@Mutex!val!7) (FRESH) (SHARED)))))) (let (_ (a!2 (ite (= (x!1) T@Mutex!val!6) (FRESH) (ite (= (x!1) T@Mutex!val!5) (FRESH) (ite (= (x!1) T@Mutex!val!19) (FRESH) (a!1)))))) (ite (= (x!1) T@Mutex!val!20) (FRESH) (a!2)))))
                        |Mutex._state@1 -> (lambda (_ (x!1 (T@Mutex))) (FRESH))
                        |Mutex._state@2 -> (_ (as-array) (k!74))
                        |Mutex._state@3 -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!50) (FRESH) (ite (= (x!1) T@Mutex!val!17) (FRESH) (ite (= (x!1) T@Mutex!val!15) (FRESH) (SHARED)))))) (ite (= (x!1) T@Mutex!val!58) (FRESH) (a!1))))
                        |Mutex._state@4 -> (lambda (_ (x!1 (T@Mutex))) (FRESH))
                        |Mutex._state187 -> (_ (as-array) (k!74))
                        |Mutex._state187_post -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!50) (FRESH) (ite (= (x!1) T@Mutex!val!17) (FRESH) (ite (= (x!1) T@Mutex!val!15) (FRESH) (SHARED)))))) (ite (= (x!1) T@Mutex!val!58) (FRESH) (a!1))))
                        |Mutex.m._nextCAS@2 -> (_ (as-array) (k!76))
                        |Mutex.m._nextCAS@3 -> (_ (as-array) (k!84))
                        |Mutex.m._nextCAS187 -> (_ (as-array) (k!76))
                        |Mutex.m._nextCAS187_post -> (_ (as-array) (k!84))
                        |Mutex.m@2 -> (lambda (_ (x!1 (T@Mutex))) 84)
                        |Mutex.m@3 -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!39) 42 (ite (= (x!1) T@Mutex!val!61) 125 (ite (= (x!1) T@Mutex!val!42) 50 108))))) (ite (= (x!1) T@Mutex!val!46) 80 (ite (= (x!1) T@Mutex!val!45) 76 (a!1)))))
                        |Mutex.m187 -> (lambda (_ (x!1 (T@Mutex))) 84)
                        |Mutex.m187_post -> (lambda (_ (x!1 (T@Mutex))) (let (_ (a!1 (ite (= (x!1) T@Mutex!val!39) 42 (ite (= (x!1) T@Mutex!val!61) 125 (ite (= (x!1) T@Mutex!val!42) 50 108))))) (ite (= (x!1) T@Mutex!val!46) 80 (ite (= (x!1) T@Mutex!val!45) 76 (a!1)))))
                        |Mutex.null -> T@Mutex!val!2
                        |PhaseError -> T@Phase!val!2
                        |Test._mutex -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!29) 21 (ite (= (x!1) T@Test!val!30) 24 (ite (= (x!1) T@Test!val!31) 27 1))))) (ite (= (x!1) T@Test!val!44) 117 (ite (= (x!1) T@Test!val!32) 30 (a!1)))))
                        |Test._mutex@0 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!47) 140 (ite (= (x!1) T@Test!val!48) 143 (ite (= (x!1) T@Test!val!46) 137 1))))) (ite (= (x!1) T@Test!val!44) 116 (ite (= (x!1) T@Test!val!45) 134 (a!1)))))
                        |Test._mutex@2 -> (_ (as-array) (k!81))
                        |Test._mutex@3 -> (_ (as-array) (k!89))
                        |Test._mutex187 -> (_ (as-array) (k!81))
                        |Test._mutex187_post -> (_ (as-array) (k!89))
                        |Test._state -> (lambda (_ (x!1 (T@Test))) (ite (= (x!1) T@Test!val!3) (FRESH) (ite (= (x!1) T@Test!val!4) (FRESH) (ite (= (x!1) T@Test!val!2) (FRESH) (SHARED)))))
                        |Test._state@0 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!23) (FRESH) (ite (= (x!1) T@Test!val!19) (FRESH) (ite (= (x!1) T@Test!val!7) (FRESH) (SHARED)))))) (let (_ (a!2 (ite (= (x!1) T@Test!val!18) (FRESH) (ite (= (x!1) T@Test!val!17) (FRESH) (ite (= (x!1) T@Test!val!5) (FRESH) (a!1)))))) (ite (= (x!1) T@Test!val!6) (FRESH) (a!2)))))
                        |Test._state@1 -> (lambda (_ (x!1 (T@Test))) (FRESH))
                        |Test._state@2 -> (_ (as-array) (k!71))
                        |Test._state@3 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!16) (FRESH) (ite (= (x!1) T@Test!val!25) (FRESH) (ite (= (x!1) T@Test!val!34) (FRESH) (SHARED)))))) (ite (= (x!1) T@Test!val!14) (FRESH) (a!1))))
                        |Test._state@4 -> (lambda (_ (x!1 (T@Test))) (FRESH))
                        |Test._state187 -> (_ (as-array) (k!71))
                        |Test._state187_post -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!16) (FRESH) (ite (= (x!1) T@Test!val!25) (FRESH) (ite (= (x!1) T@Test!val!34) (FRESH) (SHARED)))))) (ite (= (x!1) T@Test!val!14) (FRESH) (a!1))))
                        |Test.m -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!26) T@Mutex!val!26 (ite (= (x!1) T@Test!val!0) T@Mutex!val!0 (ite (= (x!1) T@Test!val!4) T@Mutex!val!4 T@Mutex!val!28))))) (ite (= (x!1) T@Test!val!28) T@Mutex!val!30 (a!1))))
                        |Test.m._nextCAS@2 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!40) 67 (ite (= (x!1) T@Test!val!47) 141 (ite (= (x!1) T@Test!val!43) 87 18))))) (let (_ (a!2 (ite (= (x!1) T@Test!val!45) 133 (ite (= (x!1) T@Test!val!39) 65 (ite (= (x!1) T@Test!val!38) 61 (a!1)))))) (ite (= (x!1) T@Test!val!31) 28 (ite (= (x!1) T@Test!val!41) 72 (a!2))))))
                        |Test.m._nextCAS@3 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!40) 68 (ite (= (x!1) T@Test!val!47) 139 (ite (= (x!1) T@Test!val!43) 86 22))))) (let (_ (a!2 (ite (= (x!1) T@Test!val!45) 135 (ite (= (x!1) T@Test!val!39) 64 (ite (= (x!1) T@Test!val!38) 62 (a!1)))))) (ite (= (x!1) T@Test!val!31) 26 (ite (= (x!1) T@Test!val!41) 69 (a!2))))))
                        |Test.m._nextCAS187 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!40) 67 (ite (= (x!1) T@Test!val!47) 141 (ite (= (x!1) T@Test!val!43) 87 18))))) (let (_ (a!2 (ite (= (x!1) T@Test!val!45) 133 (ite (= (x!1) T@Test!val!39) 65 (ite (= (x!1) T@Test!val!38) 61 (a!1)))))) (ite (= (x!1) T@Test!val!31) 28 (ite (= (x!1) T@Test!val!41) 72 (a!2))))))
                        |Test.m._nextCAS187_post -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!40) 68 (ite (= (x!1) T@Test!val!47) 139 (ite (= (x!1) T@Test!val!43) 86 22))))) (let (_ (a!2 (ite (= (x!1) T@Test!val!45) 135 (ite (= (x!1) T@Test!val!39) 64 (ite (= (x!1) T@Test!val!38) 62 (a!1)))))) (ite (= (x!1) T@Test!val!31) 26 (ite (= (x!1) T@Test!val!41) 69 (a!2))))))
                        |Test.m@0 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!7) T@Mutex!val!7 (ite (= (x!1) T@Test!val!0) T@Mutex!val!0 (ite (= (x!1) T@Test!val!36) T@Mutex!val!34 T@Mutex!val!21))))) (ite (= (x!1) T@Test!val!35) T@Mutex!val!32 (ite (= (x!1) T@Test!val!26) T@Mutex!val!25 (a!1)))))
                        |Test.m@1 -> (lambda (_ (x!1 (T@Test))) T@Mutex!val!24)
                        |Test.m@2 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!0) T@Mutex!val!8 (ite (= (x!1) T@Test!val!37) T@Mutex!val!35 (ite (= (x!1) T@Test!val!27) T@Mutex!val!27 T@Mutex!val!17))))) (ite (= (x!1) T@Test!val!13) T@Mutex!val!14 (ite (= (x!1) T@Test!val!35) T@Mutex!val!31 (a!1)))))
                        |Test.m@3 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!28) T@Mutex!val!29 (ite (= (x!1) T@Test!val!0) T@Mutex!val!18 (ite (= (x!1) T@Test!val!36) T@Mutex!val!33 T@Mutex!val!36))))) (ite (= (x!1) T@Test!val!16) T@Mutex!val!17 (a!1))))
                        |Test.m@4 -> (lambda (_ (x!1 (T@Test))) T@Mutex!val!11)
                        |Test.m187 -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!0) T@Mutex!val!8 (ite (= (x!1) T@Test!val!37) T@Mutex!val!35 (ite (= (x!1) T@Test!val!27) T@Mutex!val!27 T@Mutex!val!17))))) (ite (= (x!1) T@Test!val!13) T@Mutex!val!14 (ite (= (x!1) T@Test!val!35) T@Mutex!val!31 (a!1)))))
                        |Test.m187_post -> (lambda (_ (x!1 (T@Test))) (let (_ (a!1 (ite (= (x!1) T@Test!val!28) T@Mutex!val!29 (ite (= (x!1) T@Test!val!0) T@Mutex!val!18 (ite (= (x!1) T@Test!val!36) T@Mutex!val!33 T@Mutex!val!36))))) (ite (= (x!1) T@Test!val!16) T@Mutex!val!17 (a!1))))
                        |Test.null -> T@Test!val!1
                        |this -> T@Test!val!0
                        |tid -> 0
                        |Tid.null -> 1
                        |k!71 -> {
                        |  T@Test!val!0 -> (SHARED)
                        |  T@Test!val!12 -> (SHARED)
                        |  T@Test!val!15 -> (SHARED)
                        |  T@Test!val!28 -> (SHARED)
                        |  T@Test!val!30 -> (SHARED)
                        |  T@Test!val!36 -> (SHARED)
                        |  T@Test!val!37 -> (SHARED)
                        |  T@Test!val!38 -> (SHARED)
                        |  T@Test!val!41 -> (SHARED)
                        |  T@Test!val!42 -> (SHARED)
                        |  T@Test!val!46 -> (SHARED)
                        |  else -> (FRESH)
                        |}
                        |k!74 -> {
                        |  T@Mutex!val!13 -> (SHARED)
                        |  T@Mutex!val!16 -> (SHARED)
                        |  T@Mutex!val!38 -> (SHARED)
                        |  T@Mutex!val!39 -> (SHARED)
                        |  T@Mutex!val!42 -> (SHARED)
                        |  T@Mutex!val!43 -> (SHARED)
                        |  T@Mutex!val!44 -> (SHARED)
                        |  T@Mutex!val!45 -> (SHARED)
                        |  T@Mutex!val!53 -> (SHARED)
                        |  T@Mutex!val!54 -> (SHARED)
                        |  T@Mutex!val!60 -> (SHARED)
                        |  T@Mutex!val!61 -> (SHARED)
                        |  else -> (FRESH)
                        |}
                        |k!76 -> {
                        |  T@Mutex!val!38 -> 38
                        |  T@Mutex!val!40 -> 45
                        |  T@Mutex!val!41 -> 47
                        |  T@Mutex!val!43 -> 57
                        |  T@Mutex!val!45 -> 78
                        |  T@Mutex!val!47 -> 84
                        |  T@Mutex!val!52 -> 102
                        |  T@Mutex!val!55 -> 113
                        |  T@Mutex!val!59 -> 119
                        |  T@Mutex!val!62 -> 130
                        |  else -> 41
                        |}
                        |k!77 -> {
                        |  T@Mutex!val!13 -> 2
                        |  T@Mutex!val!16 -> 15
                        |  T@Mutex!val!38 -> 37
                        |  T@Mutex!val!39 -> 92
                        |  T@Mutex!val!42 -> 51
                        |  T@Mutex!val!43 -> 55
                        |  T@Mutex!val!44 -> 59
                        |  T@Mutex!val!45 -> 94
                        |  T@Mutex!val!53 -> 105
                        |  T@Mutex!val!54 -> 144
                        |  T@Mutex!val!60 -> 122
                        |  T@Mutex!val!61 -> 146
                        |  else -> 1
                        |}
                        |k!81 -> {
                        |  T@Test!val!0 -> 7
                        |  T@Test!val!12 -> 3
                        |  T@Test!val!15 -> 14
                        |  T@Test!val!28 -> 88
                        |  T@Test!val!30 -> 23
                        |  T@Test!val!36 -> 90
                        |  T@Test!val!37 -> 34
                        |  T@Test!val!38 -> 60
                        |  T@Test!val!41 -> 70
                        |  T@Test!val!42 -> 74
                        |  T@Test!val!46 -> 136
                        |  else -> 1
                        |}
                        |k!84 -> {
                        |  T@Mutex!val!38 -> 39
                        |  T@Mutex!val!40 -> 46
                        |  T@Mutex!val!41 -> 49
                        |  T@Mutex!val!43 -> 54
                        |  T@Mutex!val!45 -> 75
                        |  T@Mutex!val!47 -> 83
                        |  T@Mutex!val!52 -> 104
                        |  T@Mutex!val!55 -> 111
                        |  T@Mutex!val!59 -> 121
                        |  T@Mutex!val!62 -> 128
                        |  else -> 44
                        |}
                        |k!85 -> {
                        |  T@Mutex!val!12 -> 96
                        |  T@Mutex!val!13 -> 11
                        |  T@Mutex!val!14 -> 97
                        |  T@Mutex!val!16 -> 5
                        |  T@Mutex!val!37 -> 36
                        |  T@Mutex!val!38 -> 40
                        |  T@Mutex!val!39 -> 93
                        |  T@Mutex!val!41 -> 48
                        |  T@Mutex!val!42 -> 52
                        |  T@Mutex!val!43 -> 56
                        |  T@Mutex!val!44 -> 58
                        |  T@Mutex!val!45 -> 95
                        |  T@Mutex!val!46 -> 79
                        |  T@Mutex!val!47 -> 82
                        |  T@Mutex!val!49 -> 99
                        |  T@Mutex!val!53 -> 107
                        |  T@Mutex!val!54 -> 145
                        |  T@Mutex!val!56 -> 114
                        |  T@Mutex!val!57 -> 118
                        |  T@Mutex!val!60 -> 124
                        |  T@Mutex!val!61 -> 147
                        |  T@Mutex!val!63 -> 131
                        |  else -> 1
                        |}
                        |k!89 -> {
                        |  T@Test!val!0 -> 8
                        |  T@Test!val!11 -> 98
                        |  T@Test!val!12 -> 12
                        |  T@Test!val!13 -> 13
                        |  T@Test!val!15 -> 6
                        |  T@Test!val!24 -> 16
                        |  T@Test!val!28 -> 89
                        |  T@Test!val!30 -> 25
                        |  T@Test!val!32 -> 29
                        |  T@Test!val!33 -> 31
                        |  T@Test!val!36 -> 91
                        |  T@Test!val!37 -> 35
                        |  T@Test!val!38 -> 63
                        |  T@Test!val!40 -> 66
                        |  T@Test!val!41 -> 71
                        |  T@Test!val!42 -> 73
                        |  T@Test!val!43 -> 85
                        |  T@Test!val!46 -> 138
                        |  T@Test!val!48 -> 142
                        |  else -> 1
                        |}
                        |tickleBool -> {
                        |  else -> true
                        |}
                        |*** END_MODEL
        """.stripMargin
      val model = BoogieModel.parse(m)
    }

    test("Big1") {
      val m =         """  *** MODEL
                        |  %lbl%@1 -> false
                        |  %lbl%+0 -> true
                        |  %lbl%+10 -> true
                        |  %lbl%+11 -> true
                        |  %lbl%+12 -> true
                        |  %lbl%+5 -> true
                        |  %lbl%+6 -> true
                        |  _B -> T@Mover!val!3
                        |  _E -> T@Mover!val!4
                        |  _i@@0!5!0 -> T@Cow!val!3
                        |  _i@@1!6!1 -> T@Cow!val!4
                        |  _L -> T@Mover!val!1
                        |  _N -> T@Mover!val!2
                        |  _pc@0 -> T@Phase!val!1
                        |  _pc@1 -> T@Phase!val!2
                        |  _R -> T@Mover!val!0
                        |  c@0 -> T@Cow!val!1
                        |  Cow._mutex -> (_ (as-array) (k!12))
                        |  Cow._state -> (_ (as-array) (k!2))
                        |  Cow._state@0 -> (_ (as-array) (k!3))
                        |  Cow.c1 -> (_ (as-array) (k!4))
                        |  Cow.c1._nextCAS -> (_ (as-array) (k!5))
                        |  Cow.c1._nextCAS@0 -> (_ (as-array) (k!6))
                        |  Cow.c1._nextCAS@1 -> (_ (as-array) (k!8))
                        |  Cow.c1._nextCAS@2 -> (_ (as-array) (k!9))
                        |  Cow.c1@0 -> (_ (as-array) (k!7))
                        |  Cow.null -> T@Cow!val!2
                        |  MatchingLeft -> T@Phase!val!0
                        |  MatchingRight -> T@Phase!val!1
                        |  mover419@0 -> T@Mover!val!3
                        |  mover422@0 -> T@Mover!val!4
                        |  PhaseError -> T@Phase!val!2
                        |  t -> 4
                        |  this -> T@Cow!val!0
                        |  tid -> 2437
                        |  Tid.null -> 1
                        |  array-ext -> {
                        |  (_ (as-array) (k!12)) (_ (as-array) (k!5)) -> T@Cow!val!5
                        |  else -> T@Cow!val!5
                        |  }
                        |  k!10 -> {
                        |  T@Cow!val!0 -> 4
                        |  T@Cow!val!1 -> 4
                        |  else -> 7
                        |  }
                        |  k!11 -> {
                        |  T@Cow!val!0 -> 1
                        |  T@Cow!val!1 -> 1
                        |  T@Cow!val!5 -> 10
                        |  else -> 8
                        |  }
                        |  k!12 -> {
                        |  T@Cow!val!1 -> 1
                        |  T@Cow!val!3 -> 1
                        |  T@Cow!val!4 -> 1
                        |  T@Cow!val!5 -> 9
                        |  else -> 6
                        |  }
                        |  k!2 -> {
                        |  T@Cow!val!0 -> (SHARED)
                        |  T@Cow!val!1 -> (FRESH)
                        |  T@Cow!val!3 -> (FRESH)
                        |  T@Cow!val!4 -> (FRESH)
                        |  T@Cow!val!5 -> (SHARED)
                        |  else -> (FRESH)
                        |  }
                        |  k!3 -> {
                        |  T@Cow!val!0 -> (SHARED)
                        |  T@Cow!val!1 -> (LOCAL 2437)
                        |  T@Cow!val!3 -> (FRESH)
                        |  T@Cow!val!4 -> (FRESH)
                        |  T@Cow!val!5 -> (SHARED)
                        |  else -> (FRESH)
                        |  }
                        |  k!4 -> {
                        |  T@Cow!val!1 -> 0
                        |  else -> 7
                        |  }
                        |  k!5 -> {
                        |  T@Cow!val!0 -> 5
                        |  T@Cow!val!1 -> 2
                        |  T@Cow!val!5 -> 10
                        |  else -> 8
                        |  }
                        |  k!6 -> {
                        |  T@Cow!val!0 -> 5
                        |  T@Cow!val!1 -> 2437
                        |  T@Cow!val!5 -> 10
                        |  else -> 8
                        |  }
                        |  k!7 -> {
                        |  T@Cow!val!1 -> 4
                        |  else -> 7
                        |  }
                        |  k!8 -> {
                        |  T@Cow!val!0 -> 5
                        |  T@Cow!val!1 -> 1
                        |  T@Cow!val!5 -> 10
                        |  else -> 8
                        |  }
                        |  k!9 -> {
                        |  T@Cow!val!0 -> 2437
                        |  T@Cow!val!1 -> 1
                        |  T@Cow!val!5 -> 10
                        |  else -> 8
                        |  }
                        |  tickleBool -> {
                        |  false -> true
                        |  true -> true
                        |  else -> true
                        |  }
                        |  *** END_MODEL
                        |""".stripMargin
      val model = BoogieModel.parse(m)
    }

    test("Bad on Ubuntu") {
      val m =
        """
          |*** MODEL
          |$recorded.state1235 -> 1
          |$recorded.state1251 -> 1
          |$recorded.state1252 -> 0
          |%lbl%@1 -> false
          |%lbl%@2 -> false
          |%lbl%@4 -> false
          |%lbl%@7 -> false
          |%lbl%@9 -> false
          |%lbl%+0 -> true
          |%lbl%+10 -> true
          |%lbl%+11 -> true
          |%lbl%+12 -> true
          |%lbl%+3 -> true
          |%lbl%+5 -> false
          |%lbl%+6 -> true
          |%lbl%+8 -> false
          |_B -> T@Mover!val!0
          |_E -> T@Mover!val!3
          |_i!4!0 -> T@Bad!val!2
          |_L -> T@Mover!val!2
          |_N -> T@Mover!val!4
          |_pc@0 -> T@Phase!val!2
          |_pc@1 -> T@Phase!val!1
          |_pc1235 -> T@Phase!val!0
          |_pc1251 -> T@Phase!val!2
          |_R -> T@Mover!val!1
          |Bad._lock -> (_ (as-array) (k!3))
          |Bad._lock1235 -> (_ (as-array) (k!3))
          |Bad._lock1251 -> (_ (as-array) (k!3))
          |Bad._lock1252 -> (_ (as (const) (Array (T@Bad) (Int))) 0)
          |Bad._state -> (_ (as-array) (k!1))
          |Bad._state1235 -> (_ (as-array) (k!1))
          |Bad._state1251 -> (_ (as-array) (k!1))
          |Bad._state1252 -> (_ (as (const) (Array (T@Bad) (T@State))) (FRESH))
          |Bad.null -> T@Bad!val!1
          |Bad.y -> (_ (as-array) (k!2))
          |Bad.y@0 -> (_ (as (const) (Array (T@Bad) (Int))) 0)
          |Bad.y1235 -> (_ (as-array) (k!2))
          |Bad.y1251 -> (_ (as-array) (k!2))
          |Bad.y1252 -> (_ (as (const) (Array (T@Bad) (Int))) 0)
          |mover1235@0 -> T@Mover!val!4
          |mover1251@0 -> T@Mover!val!4
          |moverPath1235@0 -> (moverPath T@Mover!val!4 0)
          |moverPath1251@0 -> (moverPath T@Mover!val!4 0)
          |path1235@0 -> 0
          |path1251@0 -> 0
          |PhaseError -> T@Phase!val!1
          |PostCommit -> T@Phase!val!2
          |PreCommit -> T@Phase!val!0
          |this -> T@Bad!val!0
          |this1235 -> T@Bad!val!0
          |this1251 -> T@Bad!val!0
          |this1252 -> T@Bad!val!1
          |tid -> 0
          |Tid.null -> (- 1)
          |tid1235 -> 0
          |tid1251 -> 0
          |tid1252 -> 0
          |tmp1 -> 9
          |tmp1@0 -> (- 562)
          |tmp11235 -> 9
          |tmp11251 -> (- 562)
          |tmp11252 -> 0
          |tmp2 -> 8
          |tmp2@0 -> (- 563)
          |tmp21235 -> 8
          |tmp21251 -> (- 563)
          |tmp21252 -> 0
          |tmp31251 -> 1
          |tmp31252 -> 0
          |_trigger -> {
          |  0 -> true
          |  1 -> true
          |  2 -> true
          |  3 -> true
          |  else -> true
          |}
          |k!1 -> {
          |  T@Bad!val!0 -> (SHARED)
          |  T@Bad!val!2 -> (FRESH)
          |  else -> (LOCAL 5)
          |}
          |k!2 -> {
          |  T@Bad!val!0 -> (- 563)
          |  else -> 6
          |}
          |k!3 -> {
          |  else -> 7
          |}
          |k!4 -> {
          |  T@Bad!val!0 -> (- 562)
          |  else -> 6
          |}
          |tickleBool -> {
          |  false -> true
          |  true -> true
          |  else -> true
          |}
          |*** END_MODEL
          |
        """.stripMargin
      val model = BoogieModel.parse(m)
      print(model.v.mkString("\n"))
    }
  }

