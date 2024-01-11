module Subtle_crypto = struct
  let encrypt s a k data =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.encrypt s a k data)

  let decrypt s a k data =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.decrypt s a k data)

  let digest s a data =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.digest s a data)

  let sign s a k data =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.sign s a k data)

  let verify s a k ~sig' data =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.verify s a k ~sig' data)

  let generate_key s a ~extractable ~usages =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.generate_key s a ~extractable ~usages)

  let generate_key_pair s a ~extractable ~usages =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.generate_key_pair s a ~extractable ~usages)

  let derive_bits s a k l =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.derive_bits s a k l)

  let derive_key s a k ~derived ~extractable ~usages =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.derive_key s a k ~derived ~extractable
         ~usages)

  let export_key s f k =
    Eio_fut.await_exn (Brr_webcrypto.Subtle_crypto.export_key s f k)

  let import_key s f k a ~extractable ~usages =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.import_key s f k a ~extractable ~usages)

  let wrap_key s f k ~wrap_key ~wrapper =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.wrap_key s f k ~wrap_key ~wrapper)

  let unwrap_key s f b ~wrap_key ~wrapper ~unwrapped ~extractable ~usages =
    Eio_fut.await_exn
      (Brr_webcrypto.Subtle_crypto.unwrap_key s f b ~wrap_key ~wrapper
         ~unwrapped ~extractable ~usages)
end
