import { DisplayScatter3d } from "../show_scatter_3d";
import { DisplayScatterConfig } from "../show_scatter/show_scatter";
import * as tf from "@tensorflow/tfjs-core";
import { Tensor2D } from "@tensorflow/tfjs-core";

interface DisplaySlice3dConfig extends DisplayScatterConfig {
  epsilon: number;
  anchor: number[];
}

export class DisplaySlice3d extends DisplayScatter3d {
  protected config: DisplaySlice3dConfig;
  private anchor: Tensor2D;

  constructor(containerElement: HTMLDivElement, width: number, height: number, shinyMode: boolean) {
    super(containerElement, width, height, shinyMode);

    // hide select and brush buttons
    this.selectButtonAction = null;
    this.brushButtonAction = null;
  }

  protected preConstructPlotCallback(): void {
    this.anchor = tf.expandDims(tf.tensor(this.config.anchor), 0);
  }

  protected project(X: tf.Tensor2D, A: tf.Tensor2D): tf.Tensor2D {
    const projected = tf.matMul(X, A);

    // update point alphas for slice tour
    // subtracting the anchor vector from each point vector is the equivalent of
    // offsetting the projection plane so it passes through the anchor
    // I.e. we offset the points by -anchor instead of the projection plane by + anchor
    const X_prime = tf.sub(X, this.anchor);
    const dists = tf
      .sqrt(
        tf.sum(
          tf.square(
            tf.sub(X_prime, tf.matMul(X_prime, tf.matMul(A, tf.transpose(A))))
          ),
          1
        )
      )
      .dataSync();

    this.pointAlphas.set(
      dists.map((x) => {
        if (x < this.config.epsilon) {
          return 1;
        } else {
          return 0.1;
        }
      })
    );
    this.pointAlphas.needsUpdate = true;

    return projected as tf.Tensor2D;
  }
}
