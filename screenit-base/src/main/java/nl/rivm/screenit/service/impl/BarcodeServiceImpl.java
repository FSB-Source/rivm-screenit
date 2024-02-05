package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.BarcodeService;

import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.krysalis.barcode4j.impl.AbstractBarcodeBean;
import org.krysalis.barcode4j.impl.code128.Code128Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BarcodeServiceImpl implements BarcodeService
{
	@Override
	public InputStream maakBarcodeInputStreamVoorBrief(String message, Double barcodeHeight, AbstractBarcodeBean barcodeBean) throws IOException
	{
		return maakBarcodeInputStream(message, barcodeHeight, barcodeBean, 300);
	}

	@Override
	public InputStream maakBarcodeInputStreamVoorBrief(String message, AbstractBarcodeBean barcodeBean) throws IOException
	{
		return maakBarcodeInputStream(message, null, barcodeBean, 300);
	}

	@Override
	public InputStream maakBarcodeInputStreamVoorEmail(String message, AbstractBarcodeBean barcodeBean) throws IOException
	{
		return maakBarcodeInputStream(message, null, barcodeBean, 150);
	}

	@NotNull
	private ByteArrayInputStream maakBarcodeInputStream(String message, Double barcodeHeight, AbstractBarcodeBean barcodeBean, int resolutie) throws IOException
	{

		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		if (StringUtils.isNotBlank(message))
		{
			BitmapCanvasProvider canvas = new BitmapCanvasProvider(outputStream, "image/x-png", resolutie, BufferedImage.TYPE_BYTE_BINARY, false, 0);
			barcodeBean = setupBarcodeBean(barcodeHeight, barcodeBean);
			try
			{
				barcodeBean.generateBarcode(canvas, message);
				canvas.finish();
			}
			catch (IllegalArgumentException iae)
			{
				LOG.error(iae.getMessage(), iae);
			}
		}
		return new ByteArrayInputStream(outputStream.toByteArray());
	}

	private AbstractBarcodeBean setupBarcodeBean(Double barcodeHeight, AbstractBarcodeBean barcodeBean)
	{
		if (barcodeBean == null)
		{
			barcodeBean = new Code128Bean();
		}
		if (barcodeHeight != null)
		{
			barcodeBean.setBarHeight(barcodeHeight);
		}
		return barcodeBean;
	}
}
