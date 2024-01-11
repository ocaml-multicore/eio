open Brr

(** [SubtleCrypto] objects *)
module Subtle_crypto : sig
  val encrypt :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    ('a, 'b) Tarray.t ->
    Tarray.Buffer.t
  (** [encrypt s a k data] is [data]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/encrypt}
      encrypted} with key [k] and algorithm [a]. *)

  val decrypt :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    ('a, 'b) Tarray.t ->
    Tarray.Buffer.t
  (** [decrypt s a k data] is [data]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/decrypt}
      decrypted} with key [k] and algorithm [a]. *)

  val digest :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    ('a, 'b) Tarray.t ->
    Tarray.Buffer.t
  (** [digest s a data] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest}
      digest} of [data] according to algorithm [a]. *)

  (** {1:sign Signatures} *)

  val sign :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    ('a, 'b) Tarray.t ->
    Tarray.Buffer.t
  (** [sign s a k data] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/sign}
      signature} of [data] with key [k] and algorithm [a]. *)

  val verify :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    sig':('a, 'b) Tarray.t ->
    ('c, 'd) Tarray.t ->
    bool
  (** [verify s a k ~sig' data] is [true] iff the signature of [data]
      with key [k] and algorithm [a]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/verify}
      matches} [sig']. *)

  (** {1:gen Key generation} *)

  val generate_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    extractable:bool ->
    usages:Brr_webcrypto.Crypto_key.Usage.t list ->
    Brr_webcrypto.Crypto_algo.t
  (** [generate_key s a ~extractable ~usage] is a key
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/generateKey}generated} for algorithm [a] and usages [usages]. {b Warning}
      if the algorithm generates a key pair use {!generate_key_pair}. *)

  val generate_key_pair :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    extractable:bool ->
    usages:Brr_webcrypto.Crypto_key.Usage.t list ->
    Brr_webcrypto.Crypto_key.pair
  (** [generate_key_pair s a ~extractable ~usage] is a key
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/generateKey}generated} of type and parameters [a] and usages [usages]. {b Warning} if
      the algorithm generates a single key use {!generate_key}. *)

  (** {1:derive Key derivation} *)

  val derive_bits :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    int ->
    Tarray.Buffer.t
  (** [derive_bits s a k l] are [l] bits
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveBits}derived} from [k] with algorithm [a]. *)

  val derive_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_algo.t ->
    Brr_webcrypto.Crypto_algo.t ->
    derived:Brr_webcrypto.Crypto_algo.t ->
    extractable:bool ->
    usages:Brr_webcrypto.Crypto_key.Usage.t list ->
    Brr_webcrypto.Crypto_algo.t
  (** [derive_key s a k ~derived_type ~extractable ~usages] is a key
      of type and parameters [~derived] and usages [usages]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey}derived} from key [k] of type and parameters [a]. *)

  (** {1:codec Key encoding and decoding} *)

  val export_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_key.Format.t ->
    Brr_webcrypto.Crypto_algo.t ->
    [ `Buffer of Tarray.Buffer.t | `Json_web_key of Json.t ]
  (** [export_key s f k] is the key [k]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/exportKey}exported} in format [f]. [`Json_web_key] is only returned if
      {!Brr_webcrypto.Crypto_algo.Format.jwk} is specified. *)

  val import_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_key.Format.t ->
    [ `Buffer of Tarray.Buffer.t | `Json_web_key of Json.t ] ->
    Brr_webcrypto.Crypto_algo.t ->
    extractable:bool ->
    usages:Brr_webcrypto.Crypto_key.Usage.t list ->
    Brr_webcrypto.Crypto_algo.t
  (** [import_key s f k a ~extractable ~usages] is the key [k]
      {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/importKey}imported} from format [f] and type [a] used for [usages]. *)

  val wrap_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_key.Format.t ->
    Brr_webcrypto.Crypto_algo.t ->
    wrap_key:Brr_webcrypto.Crypto_algo.t ->
    wrapper:Brr_webcrypto.Crypto_algo.t ->
    Tarray.Buffer.t
  (** [wrap_key s f k ~wrap_key ~wrapper] is like {!export_key}
      but {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/wrapKey}encrypts} the result with [wrap_key] ad algorithm [wrapper]. *)

  val unwrap_key :
    Brr_webcrypto.Subtle_crypto.t ->
    Brr_webcrypto.Crypto_key.Format.t ->
    ('a, 'b) Tarray.t ->
    wrap_key:Brr_webcrypto.Crypto_algo.t ->
    wrapper:Brr_webcrypto.Crypto_algo.t ->
    unwrapped:Brr_webcrypto.Crypto_algo.t ->
    extractable:bool ->
    usages:Brr_webcrypto.Crypto_key.Usage.t list ->
    Brr_webcrypto.Crypto_algo.t
  (** [unwrap_key s f b ~wrap_key ~wrapper ~unwrapped ~extractable ~usages]
      is like {!import_key} but {{:https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/unwrapKey}unwraps} the wrapper of [b] made wtih [wrap_key]
      and algorithm [wrapped]. *)
end
