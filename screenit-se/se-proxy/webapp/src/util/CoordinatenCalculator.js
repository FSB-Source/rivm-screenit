/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

export const originalImageScaleFactor: number = (730 / 1084); 
const maxX: number = 100;

export const convertXPixelsToXCoordinate = (xPixels: number, imageWidth: number, iconWidth: number) => {
    return xPixels / scaleFactor(imageWidth) + scaledIconValue(iconWidth, imageWidth);
};

export const convertXPixelsToXCoordinateOriginRightUpperCorner = (xPixels: number, imageWidth: number, iconWidth: number) => {
    return xPixels / scaleFactor(imageWidth) + scaledIconValueOriginRightUpperCorner(iconWidth, imageWidth);
};

export const convertYPixelsToYCoordinate = (yPixels: number, imageWidth: number, iconHeight: number) => {
    return yPixels / scaleFactor(imageWidth) + scaledIconValue(iconHeight, imageWidth);
};

export const convertYPixelsToYCoordinateOriginRightUpperCorner = (yPixels: number, imageWidth: number) => {
    return yPixels / scaleFactor(imageWidth);
};

export const convertXCoordinateToXPixels = (xCoordinate: number, imageWidth: number, iconWidth: number) => {
    return xCoordinate * scaleFactor(imageWidth) - iconWidth / 2;
};

export const convertXCoordinateToXPixelsOriginRightUpperCorner = (xCoordinate: number, imageWidth: number, iconWidth: number) => {
    return xCoordinate * scaleFactor(imageWidth) - iconWidth;
};

export const convertYCoordinateToYPixels = (yCoordinate: number, imageWidth: number, iconHeight: number) => {
    return yCoordinate * scaleFactor(imageWidth) - iconHeight / 2;
};

export const convertYCoordinateToYPixelsOriginRightUpperCorner = (yCoordinate: number, imageWidth: number) => {
    return yCoordinate * scaleFactor(imageWidth);
};

const scaledIconValue = (iconValue: number, imageWidth: number) => {
    return iconValue / scaleFactor(imageWidth) / 2;
};

const scaledIconValueOriginRightUpperCorner = (iconValue: number, imageWidth: number) => {
    return (iconValue) / scaleFactor(imageWidth);
};

const scaleFactor = (imageWidth: number) => {
    return imageWidth / maxX;
};

export const getMaxYCoordinaat = (imageWidth: number, imageHeight: number) => {
    return imageHeight / scaleFactor(imageWidth);
};

export function getWidth(aanzichtId: string): number {
    const element: HTMLElement | null = document ? document.getElementById(aanzichtId) ? document.getElementById(aanzichtId) : null : null;
    if (element) {
        return element.clientWidth;
    }
    return 0;
}

export function getHeight(aanzichtId: string): number {
    const element: HTMLElement | null = document ? document.getElementById(aanzichtId) ? document.getElementById(aanzichtId) : null : null;
    if (element) {
        return element.clientHeight;
    }
    return 0;
}
