package scalashop

import java.util.concurrent._
import scala.collection._
import munit._

class BlurSuite extends FunSuite {

  test("boxBlurKernel should correctly compute a pixel value") {

    // Create a dummy 3x3 image with the following layout:
    // 1 2 3
    // 4 5 6
    // 7 8 9
    val img = new Img(3, 3)
    var value = 1
    for (y <- 0 until 3; x <- 0 until 3) {
      img(x, y) = rgba(value, value, value, value)
      value += 1
    }

    // Test for the corner pixel (0, 0)
    var result = boxBlurKernel(img, 0, 0, 1)
    var expected = rgba((1+2+4+5)/4, (1+2+4+5)/4, (1+2+4+5)/4, (1+2+4+5)/4)
    assertEquals(result, expected)

    // Test for a pixel at the center (1, 1)
    result = boxBlurKernel(img, 1, 1, 1)
    expected = rgba((1+2+3+4+5+6+7+8+9)/9, (1+2+3+4+5+6+7+8+9)/9, (1+2+3+4+5+6+7+8+9)/9, (1+2+3+4+5+6+7+8+9)/9)
    assertEquals(result, expected)
  }

  test("blur on a 3x3 image") {
    // Setup
    val src = new Img(3, 3)
    val dst = new Img(3, 3)
    // initialize src (replace with your actual initialization)
    for (x <- 0 until 3; y <- 0 until 3) src.update(x, y, x + y)

    // Action
    VerticalBoxBlur.blur(src, dst, 0, 3, 1)

    // Assertion
    // Check a few points
    assertEquals(dst(0, 0), boxBlurKernel(src, 0, 0, 1))
    assertEquals(dst(1, 1), boxBlurKernel(src, 1, 1, 1))
    assertEquals(dst(2, 2), boxBlurKernel(src, 2, 2, 1))

    // or check all points
    for (x <- 0 until 3; y <- 0 until 3) {
      assertEquals(dst(x, y), boxBlurKernel(src, x, y, 1))
    }
  }
}

