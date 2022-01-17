package nl.rivm.screenit.mamma.se.dto.onderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class DoorsnedeAfbeeldingenSeDto
{
	private AnnotatieAfbeeldingSeDto rechtsVerticaleDoorsnede;

	private AnnotatieAfbeeldingSeDto linksVerticaleDoorsnede;

	private AnnotatieAfbeeldingSeDto rechtsHorizontaleDoorsnede;

	private AnnotatieAfbeeldingSeDto linksHorizontaleDoorsnede;

	public AnnotatieAfbeeldingSeDto getRechtsVerticaleDoorsnede()
	{
		return rechtsVerticaleDoorsnede;
	}

	public void setRechtsVerticaleDoorsnede(AnnotatieAfbeeldingSeDto rechtsVerticaleDoorsnede)
	{
		this.rechtsVerticaleDoorsnede = rechtsVerticaleDoorsnede;
	}

	public AnnotatieAfbeeldingSeDto getLinksVerticaleDoorsnede()
	{
		return linksVerticaleDoorsnede;
	}

	public void setLinksVerticaleDoorsnede(AnnotatieAfbeeldingSeDto linksVerticaleDoorsnede)
	{
		this.linksVerticaleDoorsnede = linksVerticaleDoorsnede;
	}

	public AnnotatieAfbeeldingSeDto getRechtsHorizontaleDoorsnede()
	{
		return rechtsHorizontaleDoorsnede;
	}

	public void setRechtsHorizontaleDoorsnede(AnnotatieAfbeeldingSeDto rechtsHorizontaleDoorsnede)
	{
		this.rechtsHorizontaleDoorsnede = rechtsHorizontaleDoorsnede;
	}

	public AnnotatieAfbeeldingSeDto getLinksHorizontaleDoorsnede()
	{
		return linksHorizontaleDoorsnede;
	}

	public void setLinksHorizontaleDoorsnede(AnnotatieAfbeeldingSeDto linksHorizontaleDoorsnede)
	{
		this.linksHorizontaleDoorsnede = linksHorizontaleDoorsnede;
	}
}
