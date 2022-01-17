export type Popup = {
	titel: string;
	body: Record<string, any>;
	visible: boolean;
	callback?: (...args: Array<any>) => any;
	cancelCallback?: ((...args: Array<any>) => any);
	akkoordString?: string;
	annulerenString?: string;
	alleenOnline: boolean,
};