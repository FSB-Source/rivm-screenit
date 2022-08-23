import React from "react"

export type Popup = {
	titel: string;
	body: React.ReactNode;
	visible: boolean;
	callback?: (...args: Array<any>) => any;
	cancelCallback?: ((...args: Array<any>) => any);
	akkoordString?: string;
	annulerenString?: string;
	alleenOnline: boolean,
};