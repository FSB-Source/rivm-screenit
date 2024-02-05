package nl.rivm.screenit.main.web.component;

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

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

import nl.rivm.screenit.Constants;

import org.apache.wicket.util.convert.converter.DoubleConverter;

public class ScreenITDoubleConverter extends DoubleConverter
{
	@Override
	protected NumberFormat newNumberFormat(Locale locale)
	{
		NumberFormat nf = NumberFormat.getNumberInstance(Constants.LOCALE_NL);
		DecimalFormat df = (DecimalFormat) nf;
		df.applyPattern("##0.00");
		return df;
	}
}
