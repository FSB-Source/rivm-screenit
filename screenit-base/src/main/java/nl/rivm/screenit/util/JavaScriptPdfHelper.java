package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class JavaScriptPdfHelper
{

	private JavaScriptPdfHelper()
	{
	}

	public static String getPrintJavascript()
	{
		StringBuilder javascript = new StringBuilder();
		javascript.append("var pp = this.getPrintParams();");

		javascript.append("pp.fileName = \"\";");

		javascript.append("pp.bitmapDPI = 600;");

		javascript.append("this.print(pp);");

		return javascript.toString();
	}
}
