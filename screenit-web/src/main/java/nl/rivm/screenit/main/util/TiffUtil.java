package nl.rivm.screenit.main.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

public class TiffUtil
{

	public static PDDocument tiffToPdfDocument(File tiffFile) throws IOException
	{

		PDDocument pdDocument = new PDDocument();

		try (ImageInputStream is = ImageIO.createImageInputStream(tiffFile))
		{
			if (is == null || is.length() == 0)
			{
				throw new IOException("Kon Tiff bestand niet vinden of tiff bestand is leeg.");
			}

			Iterator<ImageReader> iterator = ImageIO.getImageReaders(is);
			if (iterator == null || !iterator.hasNext())
			{
				throw new IOException("File format wordt niet ondersteund.");
			}
			ImageReader reader = iterator.next();
			reader.setInput(is);

			int aantalImages = reader.getNumImages(true);

			for (int i = 0; i < aantalImages; i++)
			{

				BufferedImage image = reader.read(i);
				final int width = image.getWidth();
				final int height = image.getHeight();
				PDPage page = new PDPage(new PDRectangle(width, height));
				pdDocument.addPage(page);
				final PDImageXObject imageXObject = LosslessFactory.createFromImage(pdDocument, image);
				try (PDPageContentStream contentStream = new PDPageContentStream(pdDocument, page))
				{
					contentStream.drawImage(imageXObject, 0, 0, width, height);
				}
			}
		}

		return pdDocument;
	}
}
