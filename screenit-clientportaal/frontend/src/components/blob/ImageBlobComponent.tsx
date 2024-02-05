/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React from "react"
import styles from "./ImageBlobComponent.module.scss"
import classNames from "classnames"

export type ImageBlobComponentProps = {
	className?: string,
	image: string
}

const ImageBlobComponent = (props: ImageBlobComponentProps) => {
	return (
		<svg className={classNames(styles.style, props.className)}
			 xmlns="http:
			 viewBox="0 0 382 311.967">
			<defs>
				<pattern id="img1"
						 patternUnits="userSpaceOnUse"
						 width="100%"
						 height="100%">
					<image href={props.image}
						   x="-40"
						   y="-40"
						   width="125%"
						   height="125%"/>
				</pattern>
			</defs>
			<path id="Fill_843"
				  data-name="Fill 843"
				  d="M95.516,268.738A92.465,92.465,0,0,0,230.6,292.451c28.968-22.624,86.872-67.913,115.813-90.568A92.811,92.811,0,0,0,306.5,37.629C257.323,28.537,158.905,10.72,109.738,1.595A94.119,94.119,0,0,0,92.555,0,92.782,92.782,0,0,0,15.242,143.733c20.247,31.136,60.324,93.675,80.273,125"
				  fill="url(#img1)"/>
		</svg>
	)
}

export default ImageBlobComponent
