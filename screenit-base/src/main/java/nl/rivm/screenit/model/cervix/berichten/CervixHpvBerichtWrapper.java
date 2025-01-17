package nl.rivm.screenit.model.cervix.berichten;

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

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.v251.datatype.HD;
import ca.uhn.hl7v2.model.v251.datatype.IS;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.model.v251.segment.MSH;

public class CervixHpvBerichtWrapper
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvBerichtWrapper.class);

	private static String POSCONTROL = "POSCONTROL";

	private static String NEGCONTROL = "NEGCONTROL";

	private final OUL_R22 message;

	private List<OUL_R22_SPECIMEN> controles = new ArrayList<>();

	private List<CervixHpvMonsterWrapper> results = new ArrayList<CervixHpvMonsterWrapper>();

	public CervixHpvBerichtWrapper(OUL_R22 message) throws HL7Exception
	{
		this.message = message;
		splitsResults();
	}

	private void splitsResults() throws HL7Exception
	{
		List<CervixHpvMonsterWrapper> results = new ArrayList<CervixHpvMonsterWrapper>();
		List<OUL_R22_SPECIMEN> allSpecimen = message.getSPECIMENAll();
		for (OUL_R22_SPECIMEN specimen : allSpecimen)
		{
			CervixHpvMonsterWrapper cervixHpvMonsterWrapper = new CervixHpvMonsterWrapper(specimen);
			if (!isControleWaarde(cervixHpvMonsterWrapper))
			{
				results.add(cervixHpvMonsterWrapper);
			}
		}
		this.results = results;
	}

	private boolean isControleWaarde(CervixHpvMonsterWrapper cervixHpvMonsterWrapper)
	{
		if (NEGCONTROL.equals(cervixHpvMonsterWrapper.getControleWaarde()) || POSCONTROL.equals(cervixHpvMonsterWrapper.getControleWaarde())
			|| cervixHpvMonsterWrapper.getBarcode().toUpperCase().startsWith("Q"))
		{
			controles.add(cervixHpvMonsterWrapper.getSpecimen());
			return true;
		}
		return false;
	}

	public String getMessageId()
	{
		MSH header = message.getMSH();
		ST messageId = header.getMsh10_MessageControlID();
		return messageId.getValue();
	}

	public String getInstrumentId()
	{
		MSH header = message.getMSH();
		HD sendingApplication = header.getMsh3_SendingApplication();
		ST instrumentId = sendingApplication.getHd2_UniversalID();
		return instrumentId.getValue();
	}

	public String getLabnaam()
	{
		MSH header = message.getMSH();
		HD sendingFacility = header.getMsh4_SendingFacility();
		IS labNaam = sendingFacility.getHd1_NamespaceID();
		return labNaam.getValue();
	}

	public List<CervixHpvMonsterWrapper> getResultaten()
	{
		return results;
	}

	public List<OUL_R22_SPECIMEN> getControles()
	{
		return controles;
	}

	public boolean isValid()
	{
		LOG.debug("Start bericht validatie");
		LOG.debug("MessageID: " + getMessageId());
		LOG.debug("Labnaam: " + getLabnaam());
		LOG.debug("Instrument ID: " + getInstrumentId());
		LOG.debug("Aantal uitslagen: " + results.size());
		if (getMessageId() != null && getInstrumentId() != null)
		{
			for (CervixHpvMonsterWrapper sample : getResultaten())
			{
				if (!sample.isValid())
				{
					LOG.debug("Bericht invalide");
					return false;
				}
			}
			LOG.debug("Bericht valide");
			return true;
		}
		LOG.debug("Bericht invalide");
		return false;
	}

	public OUL_R22 getMessage()
	{
		return message;
	}

}
