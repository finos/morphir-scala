package morphir.spikes.wasmhost.ffi

import kyo.*
import kyo.ffi.*
import kyo.ffi.Ffi.*

final class Plugin
final class CurrentPlugin
final class HostFunction
final class FunctionTable
final class CallbackValues
final class UserData
final class ErrorSlot
final class CancelHandle

/** Declarations compile separately so Mill can inspect TASTy before compiling generated implementations. */
trait ExtismBindings extends Ffi:
  def extismVersion()(using AllowUnsafe): Borrowed[String]
  @Ffi.blocking
  def extismPluginNew(
      wasm: Buffer[Byte],
      length: Long,
      functions: Handle[FunctionTable],
      count: Long,
      wasi: Boolean,
      error: Handle[ErrorSlot]
  )(using AllowUnsafe): Fiber.Unsafe[Handle[Plugin], Any]
  @Ffi.blocking
  def extismPluginCall(plugin: Handle[Plugin], name: String, input: Buffer[Byte], length: Long)(
      using AllowUnsafe
  ): Fiber.Unsafe[Int, Any]
  def extismPluginOutputLength(plugin: Handle[Plugin])(using AllowUnsafe): Long
  def extismPluginOutputData(plugin: Handle[Plugin])(using AllowUnsafe): Handle[Byte]
  def extismPluginError(plugin: Handle[Plugin])(using AllowUnsafe): Borrowed[String]
  def extismPluginFree(plugin: Handle[Plugin])(using AllowUnsafe): Unit
  def extismPluginNewErrorFree(error: Handle[Byte])(using AllowUnsafe): Unit
  def extismPluginCancelHandle(plugin: Handle[Plugin])(using AllowUnsafe): Handle[CancelHandle]
  def extismPluginCancel(handle: Handle[CancelHandle])(using AllowUnsafe): Boolean
  def extismFunctionNew(
      name: String,
      inputs: Buffer[Int],
      inputCount: Long,
      outputs: Buffer[Int],
      outputCount: Long,
      callback: (
          Handle[CurrentPlugin],
          Handle[CallbackValues],
          Long,
          Handle[CallbackValues],
          Long,
          Handle[UserData]
      ) => Unit,
      data: Handle[UserData],
      free: Handle[UserData] => Unit,
      guard: Ffi.Guard
  )(using AllowUnsafe): Handle[HostFunction]
  def extismFunctionSetNamespace(function: Handle[HostFunction], namespace: String)(using AllowUnsafe): Unit
  def extismFunctionFree(function: Handle[HostFunction])(using AllowUnsafe): Unit

object ExtismBindings extends Ffi.Config(library = "extism")
