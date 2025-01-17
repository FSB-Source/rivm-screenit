package nl.rivm.screenit.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;

public class PostcodeNlDto implements Serializable
{
	private Long accountId;

	private String deliveryId;

	private String deliveryType;

	private String productCode;

	private String productName;

	private String deliverySource;

	private String deliveryTarget;

	private String downloads;

	private String downloadUrl;

	public PostcodeNlDto(Long accountId, String deliveryId, String deliveryType, String productCode, String productName, String deliverySource, String deliveryTarget,
		String downloads, String downloadUrl)
	{
		this.accountId = accountId;
		this.deliveryId = deliveryId;
		this.deliveryType = deliveryType;
		this.productCode = productCode;
		this.productName = productName;
		this.deliverySource = deliverySource;
		this.deliveryTarget = deliveryTarget;
		this.downloads = downloads;
		this.downloadUrl = downloadUrl;
	}

	public PostcodeNlDto()
	{
	}

	public Long getAccountId()
	{
		return accountId;
	}

	public void setAccountId(Long accountId)
	{
		this.accountId = accountId;
	}

	public String getDeliveryId()
	{
		return deliveryId;
	}

	public void setDeliveryId(String deliveryId)
	{
		this.deliveryId = deliveryId;
	}

	public String getDeliveryType()
	{
		return deliveryType;
	}

	public void setDeliveryType(String deliveryType)
	{
		this.deliveryType = deliveryType;
	}

	public String getProductCode()
	{
		return productCode;
	}

	public void setProductCode(String productCode)
	{
		this.productCode = productCode;
	}

	public String getProductName()
	{
		return productName;
	}

	public void setProductName(String productName)
	{
		this.productName = productName;
	}

	public String getDeliverySource()
	{
		return deliverySource;
	}

	public void setDeliverySource(String deliverySource)
	{
		this.deliverySource = deliverySource;
	}

	public String getDeliveryTarget()
	{
		return deliveryTarget;
	}

	public void setDeliveryTarget(String deliveryTarget)
	{
		this.deliveryTarget = deliveryTarget;
	}

	public String getDownloads()
	{
		return downloads;
	}

	public void setDownloads(String downloads)
	{
		this.downloads = downloads;
	}

	public String getDownloadUrl()
	{
		return downloadUrl;
	}

	public void setDownloadUrl(String downloadUrl)
	{
		this.downloadUrl = downloadUrl;
	}
}
