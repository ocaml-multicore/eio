open Brr_webgpu

(** WebGPU objects. *)
module Gpu : sig
  (** GPU buffers. *)
  module Buffer : sig
    val map_async :
      ?size:int -> ?offset:int -> Gpu.Buffer.t -> Gpu.Buffer.Map_mode.t -> unit
    (** [map_async b] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUBuffer/mapAsync}maps} [b]. *)
  end

  (** Shader modules. *)
  module Shader_module : sig
    val get_compilation_info :
      Gpu.Shader_module.t -> Gpu.Shader_module.Compilation_info.t
    (** [get_compilation_info sm] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUShaderModule/getCompilationInfo}compilation info} of [sm]. *)
  end

  (** Queues. *)
  module Queue : sig
    val on_submitted_work_done : Gpu.Queue.t -> unit
    (** [on_submitted_work_done q] resolves when submitted work on [q] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUQueue/onSubmittedWorkDone}
        is done}. *)
  end

  (** Devices. *)
  module Device : sig
    val lost : Gpu.Device.t -> Gpu.Device.Lost_info.t
    (** [lost d] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUDevice/lost}lost} property of [d]. *)

    val pop_error_scope : Gpu.Device.t -> Gpu.Error.t option
    (** [pop_error_scope] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUDevice/popErrorScope}pops} the last error scope. *)

    val create_compute_pipeline_async :
      Gpu.Device.t ->
      Gpu.Compute_pipeline.Descriptor.t ->
      (Gpu.Compute_pipeline.t, Gpu.Pipeline_error.t) result
    (** [create_compute_pipeline_async d descr] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUDevice/createComputePipelineAsync}creates} a compute pipeline on [d] according to [descr]. *)

    val create_render_pipeline_async :
      Gpu.Device.t ->
      Gpu.Render_pipeline.Descriptor.t ->
      (Gpu.Compute_pipeline.t, Gpu.Pipeline_error.t) result
    (** [create_render_pipeline_async d descr] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUDevice/createRenderPipelineAsync}creates} a render pipeline on [d] according to [descr]. *)
  end

  (** Adapters. *)
  module Adapter : sig
    val request_device :
      ?descriptor:Gpu.Device.Descriptor.t -> Gpu.Adapter.t -> Gpu.Device.t
    (** [request_device a]
        {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUAdapter/requestDevice}requests} the device of [a]. *)

    val request_adapter_info :
      Gpu.Adapter.t -> unmask_hints:Jstr.t list -> Gpu.Adapter.Info.t
    (** [request_adapter_info a ~unmask_hints] {{:https://developer.mozilla.org/en-US/docs/Web/API/GPUAdapter/requestAdapterInfo}requests} the adapter info of [a]. *)
  end

  val request_adapter : ?opts:Gpu.opts -> Gpu.t -> Gpu.Adapter.t option
  (** [request_adapter gpu]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/GPU/requestAdapter}
      requests} an adapter from [gpu]. *)
end
