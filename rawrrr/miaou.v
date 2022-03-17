// Generator : SpinalHDL v1.6.5    git head : 13fe218147e8e77263fa80dd60b1aad6f52f1d58
// Component : miaou
// Git hash  : bf541f98488bd0b074454f23ef020e677b0a0cb1

`timescale 1ns/1ps 

module miaou (
  input               clk,
  input               reset
);


  unamed_2 sub1 (
    .clk      (clk    ), //i
    .reset    (reset  )  //i
  );

endmodule

module unamed_2 (
  input               clk,
  input               reset
);


  unamed_1 sub2 (
    .rawrr    (clk    ), //i
    .miaou    (reset  )  //i
  );

endmodule

module unamed_1 (
  input               rawrr,
  input               miaou
);


  unamed sub3 (
    .rawrr    (rawrr  ), //i
    .miaou    (miaou  )  //i
  );

endmodule

module unamed (
  input               rawrr,
  input               miaou
);

  reg                 x;

  always @(posedge rawrr or posedge miaou) begin
    if(miaou) begin
      x <= 1'b1;
    end else begin
      x <= 1'b0;
    end
  end


endmodule
