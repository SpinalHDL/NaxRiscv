//package vooxriscv.frontend
//
//import spinal.core.{Area, Component, SpinalVerilog}
//import spinal.core.fiber.Lock
//
//
//class AluPlugin extends Plugin{
//
//  val setup = create early new Area{
//    val decoder = framework.getService(classOf[DecoderPlugin])
//    decoder.lock.retain()
//    println("a1")
//  }
//
//  val logic = create late new Area{
//    println("a2")
//
//
//    setup.decoder.lock.release()
//  }
//}
//
//class DecoderPlugin extends Plugin{
//  val lock = Lock()
//
//  val setup = create early new Area{
//    println("b1")
//  }
//
//  val logic = create late new Area{
//
//    println("b2")
//    lock.await()
//    println("b3")
//  }
//}
//
//class PcPlugin2 extends Plugin{
//
//
//  val setup = create early new Area{
//    println("c1")
//    val pipeline = getService(classOf[Frontend])
//    pipeline.lock.retain()
//    println("c1a")
//  }
//
//  val logic = create late new Area{
//    println("c2")
//    setup.pipeline.lock.release()
//  }
//}
//
//object FrontendPlay extends App{
//  SpinalVerilog(new Component {
//    val config = new FrameworkConfig
//    config.plugins += new DecoderPlugin
//    config.plugins += new WordPipeline
//    config.plugins += new PcPlugin2
//    config.plugins += new AluPlugin
//    val frontend = new Framework(config)
//  })
//}
