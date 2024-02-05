package nl.rivm.screenit.service.mamma.be.verslag;

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

import java.io.InputStream;

import com.aspose.words.FieldMergingArgs;
import com.aspose.words.IFieldMergingCallback;
import com.aspose.words.ImageFieldMergingArgs;

public class MailMergeImageCallback implements IFieldMergingCallback
{
	public void fieldMerging(FieldMergingArgs e)
	{
	}

	public void imageFieldMerging(ImageFieldMergingArgs args)
	{
		if (isAfbeeldingMergeField(args))
		{
			InputStream inputStream = (InputStream) args.getFieldValue();

			args.setImageStream(inputStream);
		}
	}

	private boolean isAfbeeldingMergeField(ImageFieldMergingArgs args)
	{
		return args.getFieldName().equalsIgnoreCase(MammaLaesieTypeMergeField._AFBEELDING_LINKERBORST_HORIZONTALE_DOORSNEDE.getMergeField())
			|| args.getFieldName().equalsIgnoreCase(MammaLaesieTypeMergeField._AFBEELDING_LINKERBORST_VERTICALE_DOORSNEDE.getMergeField())
			|| args.getFieldName().equalsIgnoreCase(MammaLaesieTypeMergeField._AFBEELDING_RECHTERBORST_HORIZONTALE_DOORSNEDE.getMergeField())
			|| args.getFieldName().equalsIgnoreCase(MammaLaesieTypeMergeField._AFBEELDING_RECHTERBORST_VERTICALE_DOORSNEDE.getMergeField());
	}
}
