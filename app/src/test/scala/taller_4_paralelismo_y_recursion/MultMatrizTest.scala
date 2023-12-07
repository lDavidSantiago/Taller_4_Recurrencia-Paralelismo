/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package taller_4_paralelismo_y_recursion

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultMatrizTest extends AnyFunSuite {
  test("test 1 1x1 ") {
    val matrices = new Matrices()
    val e4 = matrices.multMatriz(Vector(Vector(1)), Vector(Vector(2)))
    assert(e4 == Vector(Vector(2)))
  }
  test("test 2 2x2 ") {
    val matrices = new Matrices()
    val e2 = matrices.multMatriz(Vector(Vector(2, 0), Vector(1, 2)), Vector(Vector(1, 2), Vector(3, 4)))
    assert(e2 == Vector(Vector(2, 4), Vector(7, 10)))
  }

  test("test 3 4x4 "){
    val matrices = new Matrices()
    val matrix1 = Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12), Vector(13, 14, 15, 16))
    val matrix2 = Vector(Vector(16, 15, 14, 13), Vector(12, 11, 10, 9), Vector(8, 7, 6, 5), Vector(4, 3, 2, 1))

    assert(matrix1.length == 4 && matrix1.forall(row => row.length == 4))
    assert(matrix2.length == 4 && matrix2.forall(row => row.length == 4))

    val result = matrices.multMatriz(matrix1, matrix2)
    val expected = Vector(Vector(80, 70, 60, 50),
      Vector(240, 214, 188, 162),
      Vector(400, 358, 316, 274),
      Vector(560, 502, 444, 386))

    assert(result == expected)
  }

  test("test 4 16x16 "){
    val matrices = new Matrices()
    val matrix1 = Vector(Vector(4, 9, 2, 11, 0, 0, 8, 10, 6, 7, 6, 5, 10, 0, 2, 3), Vector(4, 14, 6, 9, 8, 5, 5, 1, 7, 12, 13, 15, 0, 3, 7, 2), Vector(9, 8, 3, 11, 8, 15, 4, 14, 5, 8, 2, 13, 4, 13, 10, 15), Vector(11, 3, 12, 14, 1, 7, 1, 2, 1, 1,
      1, 2, 15, 4, 5, 1), Vector(4, 4, 15, 2, 9, 3, 15, 15, 6, 0, 4, 3, 3, 5, 0, 12), Vector(14, 5, 15, 13, 13, 8, 2, 4, 5, 0, 2, 9, 11, 11, 0, 1), Vector(7, 3, 5, 4, 3, 2, 14, 6, 10, 6, 8, 6, 5, 4, 0, 0), Vector(11, 15, 5, 6, 2, 14,
      0, 14, 4, 10, 0, 8, 9, 10, 0, 5), Vector(4, 6, 10, 2, 1, 8, 1, 0, 2, 3, 10, 12, 12, 10, 4, 12), Vector(2, 11, 0, 3, 3, 11, 7, 6, 12, 6, 12, 10, 0, 8, 1, 8), Vector(7, 14, 10, 1, 10, 3, 2, 14, 11, 10, 14, 12, 12, 9, 0, 12), Vector(9, 8, 3, 14, 3, 10, 3, 11, 1, 10, 0, 7, 7, 7, 0, 2), Vector(15, 2, 8, 7, 6, 0, 3, 0, 0, 2, 14, 14, 9, 14, 12, 8), Vector(9, 13, 0, 1, 0, 11, 10, 14, 3, 10, 3, 4, 2, 10, 12, 11), Vector(14, 5, 1, 13, 6, 1, 1, 8, 3, 15, 12, 15,
      3, 8, 6, 7), Vector(9, 10, 1, 15, 9, 15, 4, 10, 11, 14, 9, 12, 14, 0, 10, 5))


    val matrix2 = Vector(Vector(11, 1, 12, 8, 1, 8, 0, 8, 13, 2, 2, 4, 0, 3, 10, 3),
      Vector(11, 1, 3, 12, 2, 1, 11, 4, 8, 14, 8, 12, 6, 15, 14, 6),
      Vector(2, 7, 1, 4, 15, 6, 10, 11, 5, 10, 8, 9, 10, 1, 9, 4),
      Vector(1, 6, 6, 5, 11, 2, 1, 0, 12, 12, 2, 15, 14, 12, 2, 14),
      Vector(10, 0, 9, 10, 14, 2, 10, 7, 12, 11, 14, 6, 12, 6, 3, 15),
      Vector(6, 15, 2, 9, 3, 15, 13, 3, 3, 5, 7, 3, 9, 11, 15, 11),
      Vector(11, 4, 3, 2, 3, 7, 8, 12, 3, 0, 3, 12, 8, 10, 10, 8),
      Vector(5, 10, 0, 6, 10, 14, 9, 2, 11, 10, 3, 11, 10, 14, 0, 0),
      Vector(7, 1, 8, 7, 3, 2, 2, 8, 2, 12, 4, 7, 11, 7, 4, 13),
      Vector(3, 13, 9, 3, 5, 13, 10, 11, 0, 5, 3, 9, 9, 14, 12, 12),
      Vector(2, 11, 0, 2, 0, 13, 6, 15, 15, 14, 9, 10, 15, 2, 1, 10),
      Vector(12, 2, 5, 3, 13, 2, 12, 10, 10, 12, 6, 9, 6, 13, 6, 14),
      Vector(5, 6, 9, 10, 15, 11, 9, 15, 0, 10, 15, 15, 9, 0, 8, 2),
      Vector(6, 15, 5, 14, 0, 9, 5, 10, 9, 0, 0, 13, 4, 0, 10, 2),
      Vector(5, 4, 14, 7, 3, 12, 13, 14, 1, 2, 14, 6, 6, 13, 1, 12),
      Vector(6, 2, 0, 7, 1, 11, 9, 5, 15, 1, 3, 6, 7, 1, 11, 1)
    )

    val result = matrices.multMatriz(matrix1, matrix2)
    val expected = Vector(Vector(509, 472, 421, 504, 574, 629, 605, 664, 599, 744, 488, 903, 764, 747, 545, 631),
      Vector(745, 632, 599, 671, 672, 742, 933, 958, 862, 1017, 726, 1022, 999, 1034, 791, 1128),
      Vector(945, 931, 733, 1041, 829, 1197, 1186, 1007, 1157, 954, 784, 1242, 1123, 1196, 1073, 1090),
      Vector(431, 512, 515, 595, 684, 637, 565, 670, 546, 634, 568, 816, 672, 494, 611, 557),
      Vector(671, 553, 334, 630, 703, 788, 812, 788, 837, 708, 560, 928, 882, 659, 709, 600),
      Vector(746, 648, 654, 864, 942, 734, 797, 884, 927, 934, 730, 1066, 935, 687, 835, 869),
      Vector(575, 493, 424, 472, 495, 620, 574, 771, 574, 641, 443, 812, 731, 631, 580, 678),
      Vector(765, 811, 557, 895, 698, 953, 929, 769, 833, 875, 606, 1052, 852, 974, 1004, 738),
      Vector(600, 653, 433, 689, 610, 830, 846, 924, 761, 730, 664, 905, 783, 536, 809, 661),
      Vector(696, 666, 399, 673, 420, 787, 808, 786, 803, 814, 528, 892, 879, 843, 785, 860),
      Vector(927, 840, 635, 985, 922, 1110, 1147, 1211, 1167, 1241, 905, 1332, 1222, 975, 1021, 983),
      Vector(595, 690, 506, 682, 666, 765, 717, 619, 725, 739, 475, 942, 782, 870, 760, 723),
      Vector(742, 663, 706, 779, 680, 910, 844, 1149, 1024, 784, 760, 1054, 863, 635, 746, 859),
      Vector(790, 779, 575, 826, 437, 1078, 988, 877, 808, 638, 597, 989, 810, 1037, 947, 744),
      Vector(746, 739, 706, 728, 701, 936, 847, 979, 1068, 923, 625, 1097, 973, 978, 769, 1017),
      Vector(931, 907, 892, 982, 996, 1203, 1203, 1171, 1019, 1259, 1013, 1348, 1330, 1339, 1031, 1355))

    assert(result == expected)
  }
  test("test parallel 1 1x1 ") {
    val matrices = new Matrices()
    val e4 = matrices.multMatrizParalelo(Vector(Vector(1)), Vector(Vector(2)))
    assert(e4 == Vector(Vector(2)))
  }
  test("test parallel 2 2x2 ") {
    val matrices = new Matrices()
    val e2 = matrices.multMatrizParalelo(Vector(Vector(2, 0), Vector(1, 2)), Vector(Vector(1, 2), Vector(3, 4)))
    assert(e2 == Vector(Vector(2, 4), Vector(7, 10)))
  }

  test("test parallel 3 4x4 "){
    val matrices = new Matrices()
    val matrix1 = Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12), Vector(13, 14, 15, 16))
    val matrix2 = Vector(Vector(16, 15, 14, 13), Vector(12, 11, 10, 9), Vector(8, 7, 6, 5), Vector(4, 3, 2, 1))

    assert(matrix1.length == 4 && matrix1.forall(row => row.length == 4))
    assert(matrix2.length == 4 && matrix2.forall(row => row.length == 4))

    val result = matrices.multMatrizParalelo(matrix1, matrix2)
    val expected = Vector(Vector(80, 70, 60, 50),
      Vector(240, 214, 188, 162),
      Vector(400, 358, 316, 274),
      Vector(560, 502, 444, 386))

    assert(result == expected)
  }

  test("test parallel 4 16x16 "){
    val matrices = new Matrices()
    val matrix1 = Vector(Vector(4, 9, 2, 11, 0, 0, 8, 10, 6, 7, 6, 5, 10, 0, 2, 3), Vector(4, 14, 6, 9, 8, 5, 5, 1, 7, 12, 13, 15, 0, 3, 7, 2), Vector(9, 8, 3, 11, 8, 15, 4, 14, 5, 8, 2, 13, 4, 13, 10, 15), Vector(11, 3, 12, 14, 1, 7, 1, 2, 1, 1,
      1, 2, 15, 4, 5, 1), Vector(4, 4, 15, 2, 9, 3, 15, 15, 6, 0, 4, 3, 3, 5, 0, 12), Vector(14, 5, 15, 13, 13, 8, 2, 4, 5, 0, 2, 9, 11, 11, 0, 1), Vector(7, 3, 5, 4, 3, 2, 14, 6, 10, 6, 8, 6, 5, 4, 0, 0), Vector(11, 15, 5, 6, 2, 14,
      0, 14, 4, 10, 0, 8, 9, 10, 0, 5), Vector(4, 6, 10, 2, 1, 8, 1, 0, 2, 3, 10, 12, 12, 10, 4, 12), Vector(2, 11, 0, 3, 3, 11, 7, 6, 12, 6, 12, 10, 0, 8, 1, 8), Vector(7, 14, 10, 1, 10, 3, 2, 14, 11, 10, 14, 12, 12, 9, 0, 12), Vector(9, 8, 3, 14, 3, 10, 3, 11, 1, 10, 0, 7, 7, 7, 0, 2), Vector(15, 2, 8, 7, 6, 0, 3, 0, 0, 2, 14, 14, 9, 14, 12, 8), Vector(9, 13, 0, 1, 0, 11, 10, 14, 3, 10, 3, 4, 2, 10, 12, 11), Vector(14, 5, 1, 13, 6, 1, 1, 8, 3, 15, 12, 15,
      3, 8, 6, 7), Vector(9, 10, 1, 15, 9, 15, 4, 10, 11, 14, 9, 12, 14, 0, 10, 5))


    val matrix2 = Vector(Vector(11, 1, 12, 8, 1, 8, 0, 8, 13, 2, 2, 4, 0, 3, 10, 3),
      Vector(11, 1, 3, 12, 2, 1, 11, 4, 8, 14, 8, 12, 6, 15, 14, 6),
      Vector(2, 7, 1, 4, 15, 6, 10, 11, 5, 10, 8, 9, 10, 1, 9, 4),
      Vector(1, 6, 6, 5, 11, 2, 1, 0, 12, 12, 2, 15, 14, 12, 2, 14),
      Vector(10, 0, 9, 10, 14, 2, 10, 7, 12, 11, 14, 6, 12, 6, 3, 15),
      Vector(6, 15, 2, 9, 3, 15, 13, 3, 3, 5, 7, 3, 9, 11, 15, 11),
      Vector(11, 4, 3, 2, 3, 7, 8, 12, 3, 0, 3, 12, 8, 10, 10, 8),
      Vector(5, 10, 0, 6, 10, 14, 9, 2, 11, 10, 3, 11, 10, 14, 0, 0),
      Vector(7, 1, 8, 7, 3, 2, 2, 8, 2, 12, 4, 7, 11, 7, 4, 13),
      Vector(3, 13, 9, 3, 5, 13, 10, 11, 0, 5, 3, 9, 9, 14, 12, 12),
      Vector(2, 11, 0, 2, 0, 13, 6, 15, 15, 14, 9, 10, 15, 2, 1, 10),
      Vector(12, 2, 5, 3, 13, 2, 12, 10, 10, 12, 6, 9, 6, 13, 6, 14),
      Vector(5, 6, 9, 10, 15, 11, 9, 15, 0, 10, 15, 15, 9, 0, 8, 2),
      Vector(6, 15, 5, 14, 0, 9, 5, 10, 9, 0, 0, 13, 4, 0, 10, 2),
      Vector(5, 4, 14, 7, 3, 12, 13, 14, 1, 2, 14, 6, 6, 13, 1, 12),
      Vector(6, 2, 0, 7, 1, 11, 9, 5, 15, 1, 3, 6, 7, 1, 11, 1)
    )

    val result = matrices.multMatrizParalelo(matrix1, matrix2)
    val expected = Vector(Vector(509, 472, 421, 504, 574, 629, 605, 664, 599, 744, 488, 903, 764, 747, 545, 631),
      Vector(745, 632, 599, 671, 672, 742, 933, 958, 862, 1017, 726, 1022, 999, 1034, 791, 1128),
      Vector(945, 931, 733, 1041, 829, 1197, 1186, 1007, 1157, 954, 784, 1242, 1123, 1196, 1073, 1090),
      Vector(431, 512, 515, 595, 684, 637, 565, 670, 546, 634, 568, 816, 672, 494, 611, 557),
      Vector(671, 553, 334, 630, 703, 788, 812, 788, 837, 708, 560, 928, 882, 659, 709, 600),
      Vector(746, 648, 654, 864, 942, 734, 797, 884, 927, 934, 730, 1066, 935, 687, 835, 869),
      Vector(575, 493, 424, 472, 495, 620, 574, 771, 574, 641, 443, 812, 731, 631, 580, 678),
      Vector(765, 811, 557, 895, 698, 953, 929, 769, 833, 875, 606, 1052, 852, 974, 1004, 738),
      Vector(600, 653, 433, 689, 610, 830, 846, 924, 761, 730, 664, 905, 783, 536, 809, 661),
      Vector(696, 666, 399, 673, 420, 787, 808, 786, 803, 814, 528, 892, 879, 843, 785, 860),
      Vector(927, 840, 635, 985, 922, 1110, 1147, 1211, 1167, 1241, 905, 1332, 1222, 975, 1021, 983),
      Vector(595, 690, 506, 682, 666, 765, 717, 619, 725, 739, 475, 942, 782, 870, 760, 723),
      Vector(742, 663, 706, 779, 680, 910, 844, 1149, 1024, 784, 760, 1054, 863, 635, 746, 859),
      Vector(790, 779, 575, 826, 437, 1078, 988, 877, 808, 638, 597, 989, 810, 1037, 947, 744),
      Vector(746, 739, 706, 728, 701, 936, 847, 979, 1068, 923, 625, 1097, 973, 978, 769, 1017),
      Vector(931, 907, 892, 982, 996, 1203, 1203, 1171, 1019, 1259, 1013, 1348, 1330, 1339, 1031, 1355))

    assert(result == expected)
  }


}
