package nl.rivm.screenit.util.cervix.HpvBerichtGenerator;

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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class CervixHpvBerichtGeneratorWrapper implements Serializable
{
	
	private static final long serialVersionUID = 1L;

	private List<CervixHpvBerichtGeneratorMonsterWrapper> monsterWrappers = new ArrayList<>();

	private String instrumentId;

	private String labNaam;

	private String messageId;

	public List<CervixHpvBerichtGeneratorMonsterWrapper> getMonsterWrappers()
	{
		return monsterWrappers;
	}

	public void setMonsterWrappers(List<CervixHpvBerichtGeneratorMonsterWrapper> monsterWrappers)
	{
		this.monsterWrappers = monsterWrappers;
	}

	public String getInstrumentId()
	{
		return instrumentId;
	}

	public void setInstrumentId(String instrumentId)
	{
		this.instrumentId = instrumentId;
	}

	public String getLabNaam()
	{
		return labNaam;
	}

	public void setLabNaam(String labNaam)
	{
		this.labNaam = labNaam;
	}

	public String getMessageId()
	{
		return messageId;
	}

	public void setMessageId(String messageId)
	{
		this.messageId = messageId;
	}
}
