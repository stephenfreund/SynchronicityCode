package anchor.unit

import org.scalatest.FunSuite
import anchor.sink._
import anchor.tool._
import anchor.boogie._

import scala.io.Source

class SinkModelTests extends FunSuite {



  test("Big0") {
    val m =
      """
        |*** MODEL
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
        |_pc187 -> T@Phase!val!0
        |_pc187_post -> T@Phase!val!0
        |_R -> T@Mover!val!1
        |i@0 -> 10
        |i@2 -> 19
        |i@3 -> 20
        |i187 -> 19
        |i187_post -> 19
        |m@0 -> T@Mutex!val!0
        |m@1 -> T@Mutex!val!8
        |m187 -> T@Mutex!val!8
        |m187_post -> T@Mutex!val!8
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
        |this187 -> T@Test!val!0
        |this187_post -> T@Test!val!0
        |tid -> 0
        |Tid.null -> 1
        |tid187 -> 0
        |tid187_post -> 0
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
        |
    """.stripMargin

    val program = anchor.sink.Parser.parse(
      """
        |class Mutex {
        |  int m N#N;
        |}
        |
        |class Test {
        |  Mutex m B#N;
        |
        |  public Mutex f() {
        |    Mutex m := this.m;
        |    int i = 0;
        |    while (i < 10) {
        |      yield;
        |      i = i + 1;
        |    }
        |    while (i < 20)
        |      invariant  this.m == m;
        |    {
        |      yield;
        |      i = i + 1;
        |      m = Mutex.null;
        |    }
        |    assert this.m == m;
        |    return m;
        |  }
        |}
      """.stripMargin)
    BuildScope.annotate(program)
    TypeCheck.tc(program)

//      try {
        val model = new SinkModel(BoogieModel.parse(m), "187")
        assert(model.local("m") == Some(SinkRef(8, "Mutex")))
        assert(model.local("_pc") == Some(SinkPhase(PostCommit())))

        val ref = SinkRef(43, "Mutex")
        assert(model.field(ref, "_state") == SinkState(Shared()))
        assert(model.field(SinkRef(-33, "Mutex"), "m._nextCAS") == SinkInt(41))

        assert(model.field(SinkRef(25, "Test"), "m") == SinkRef(17, "Mutex"))
        assert(model.field(SinkRef(27, "Test"), "m") == SinkRef(27, "Mutex"))

//        println(model.objects())
//        println(model.objects("Test"))
//        println(model.dot(program))
//      } catch {
//        case anchor.util.Failure(_, message, pos) => {
//          println(s"${pos.pos}: $message")
//        }
//      }
  }

  test("Big1") {
    val fileContents = Source.fromFile("bin/p2.txt").getLines.mkString("\n")
    val model = new SinkModel(BoogieModel.parse(fileContents), "6616")

  }
}

