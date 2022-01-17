export const originalImageScaleFactor: number = 730 / 1084

const maxX = 100
export const convertXPixelsToXCoordinate = (xPixels: number, imageWidth: number, iconWidth: number): number => {
	return xPixels / scaleFactor(imageWidth) + scaledIconValue(iconWidth, imageWidth)
}
export const convertXPixelsToXCoordinateOriginRightUpperCorner = (xPixels: number, imageWidth: number, iconWidth: number): number => {
	return xPixels / scaleFactor(imageWidth) + scaledIconValueOriginRightUpperCorner(iconWidth, imageWidth)
}
export const convertYPixelsToYCoordinate = (yPixels: number, imageWidth: number, iconHeight: number): number => {
	return yPixels / scaleFactor(imageWidth) + scaledIconValue(iconHeight, imageWidth)
}
export const convertYPixelsToYCoordinateOriginRightUpperCorner = (yPixels: number, imageWidth: number): number => {
	return yPixels / scaleFactor(imageWidth)
}
export const convertXCoordinateToXPixels = (xCoordinate: number, imageWidth: number, iconWidth: number): number => {
	return xCoordinate * scaleFactor(imageWidth) - iconWidth / 2
}
export const convertXCoordinateToXPixelsOriginRightUpperCorner = (xCoordinate: number, imageWidth: number, iconWidth: number): number => {
	return xCoordinate * scaleFactor(imageWidth) - iconWidth
}
export const convertYCoordinateToYPixels = (yCoordinate: number, imageWidth: number, iconHeight: number): number => {
	return yCoordinate * scaleFactor(imageWidth) - iconHeight / 2
}
export const convertYCoordinateToYPixelsOriginRightUpperCorner = (yCoordinate: number, imageWidth: number): number => {
	return yCoordinate * scaleFactor(imageWidth)
}

const scaledIconValue = (iconValue: number, imageWidth: number): number => {
	return iconValue / scaleFactor(imageWidth) / 2
}

const scaledIconValueOriginRightUpperCorner = (iconValue: number, imageWidth: number): number => {
	return iconValue / scaleFactor(imageWidth)
}

const scaleFactor = (imageWidth: number): number => {
	return imageWidth / maxX
}

export const getMaxYCoordinaat = (imageWidth: number, imageHeight: number): number => {
	return imageHeight / scaleFactor(imageWidth)
}

export function getWidth(aanzichtId: string): number {
	const element = document?.getElementById(aanzichtId)
	if (element) {
		return element.clientWidth
	}
	return 0
}

export function getHeight(aanzichtId: string): number {
	const element = document?.getElementById(aanzichtId)
	if (element) {
		return element.clientHeight
	}
	return 0
}