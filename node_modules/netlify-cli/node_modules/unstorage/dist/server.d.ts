import { RequestListener } from 'node:http';
import { H3Event, EventHandler } from 'h3';
import { a as Storage } from './types-e3a76105.js';

type StorageServerRequest = {
    event: H3Event;
    key: string;
    type: "read" | "write";
};
interface StorageServerOptions {
    authorize?: (request: StorageServerRequest) => void | Promise<void>;
    resolvePath?: (event: H3Event) => string;
}
declare function createH3StorageHandler(storage: Storage, opts?: StorageServerOptions): EventHandler;
declare function createStorageServer(storage: Storage, options?: StorageServerOptions): {
    handle: RequestListener;
};

export { StorageServerOptions, StorageServerRequest, createH3StorageHandler, createStorageServer };
