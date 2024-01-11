open Brr_webgpu

module Gpu = struct
  module Buffer = struct
    let map_async ?size ?offset b mode =
      Eio_fut.await_exn (Gpu.Buffer.map_async ?size ?offset b mode)
  end

  module Shader_module = struct
    let get_compilation_info m =
      Eio_fut.await_exn (Gpu.Shader_module.get_compilation_info m)
  end

  module Queue = struct
    let on_submitted_work_done q =
      Eio_fut.await_exn (Gpu.Queue.on_submitted_work_done q)
  end

  module Device = struct
    let lost d = Eio_fut.await_exn (Gpu.Device.lost d)
    let pop_error_scope d = Eio_fut.await_exn (Gpu.Device.pop_error_scope d)

    let create_compute_pipeline_async d descr =
      Eio_fut.await (Gpu.Device.create_compute_pipeline_async d descr)

    let create_render_pipeline_async d descr =
      Eio_fut.await (Gpu.Device.create_render_pipeline_async d descr)
  end

  module Adapter = struct
    let request_device ?descriptor a =
      Eio_fut.await_exn (Gpu.Adapter.request_device ?descriptor a)

    let request_adapter_info a ~unmask_hints =
      Eio_fut.await_exn (Gpu.Adapter.request_adapter_info a ~unmask_hints)
  end

  let request_adapter ?opts gpu =
    Eio_fut.await_exn (Gpu.request_adapter ?opts gpu)
end
