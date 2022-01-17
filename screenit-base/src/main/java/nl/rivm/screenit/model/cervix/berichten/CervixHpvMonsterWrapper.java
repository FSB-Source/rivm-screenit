package nl.rivm.screenit.model.cervix.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;

import ca.uhn.hl7v2.model.Varies;
import ca.uhn.hl7v2.model.v251.datatype.CE;
import ca.uhn.hl7v2.model.v251.datatype.DR;
import ca.uhn.hl7v2.model.v251.datatype.EI;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_CONTAINER;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_ORDER;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_RESULT;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;
import ca.uhn.hl7v2.model.v251.segment.INV;
import ca.uhn.hl7v2.model.v251.segment.OBX;
import ca.uhn.hl7v2.model.v251.segment.SPM;

@AllArgsConstructor
@Slf4j
public class CervixHpvMonsterWrapper
{
	private final OUL_R22_SPECIMEN specimen;

	public String getBarcode()
	{
		SPM spm = specimen.getSPM();
		EI spm2 = spm.getSpm2_SpecimenID().getEip1_PlacerAssignedIdentifier();
		ST spm21 = spm2.getEi1_EntityIdentifier();
		return spm21.getValue();
	}

	public String getControleWaarde()
	{
		OUL_R22_CONTAINER container = specimen.getCONTAINER();
		INV inv = container.getINV();
		CE controle = inv.getInv1_SubstanceIdentifier();
		return controle.getCe1_Identifier().getValue();
	}

	public OUL_R22_SPECIMEN getSpecimen()
	{
		return specimen;
	}

	private String getStringValueAnalyseDatum()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		OUL_R22_RESULT result = order.getRESULT(0);
		OBX obx = result.getOBX();

		Varies obx5_0 = obx.getObx5_ObservationValue(0);
		DR data = (DR) obx5_0.getData();
		return data.getDr1_RangeStartDateTime().getTs1_Time().getValue();
	}

	public Date getAnalyseDatum()
	{
		Date analyseDatum = null;
		try
		{
			String datum = getStringValueAnalyseDatum();
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
			analyseDatum = format.parse(datum);
		}
		catch (ParseException e)
		{
			return null;
		}
		return analyseDatum;
	}

	private String getStringValueAutorisatieDatum()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		OUL_R22_RESULT result = order.getRESULT(0);
		OBX obx = result.getOBX();

		Varies obx5_0 = obx.getObx5_ObservationValue(0);
		DR data = (DR) obx5_0.getData();
		return data.getDr2_RangeEndDateTime().getTs1_Time().getValue();
	}

	public Date getAutorisatieDatum()
	{
		Date autorisatieDatum = null;
		try
		{
			String datum = getStringValueAutorisatieDatum();
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
			autorisatieDatum = format.parse(datum);
		}
		catch (ParseException e)
		{
			return null;
		}
		return autorisatieDatum;
	}

	private String getStringResultValue(int resultaatNr)
	{
		OUL_R22_ORDER order = specimen.getORDER();
		if (order.getRESULTReps() > resultaatNr)
		{
			OUL_R22_RESULT result = order.getRESULT(resultaatNr);
			OBX obx = result.getOBX();
			Varies obx5 = obx.getObx5_ObservationValue(0);
			if (obx5.getData() instanceof ST)
			{
				ST data = (ST) obx5.getData();
				return data.getValue();
			}
		}
		return null;
	}

	private String getStringResultCode(int resultaatNr)
	{
		OUL_R22_ORDER order = specimen.getORDER();
		if (order.getRESULTReps() > resultaatNr)
		{
			OUL_R22_RESULT result = order.getRESULT(resultaatNr);
			OBX obx = result.getOBX();
			ST st = obx.getObx3_ObservationIdentifier().getCe1_Identifier();
			return st.getValue();
		}
		else
		{
			return null;
		}
	}

	private String getStringOrderCode()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		ST st = order.getOBR().getObr4_UniversalServiceIdentifier().getCe1_Identifier();
		return st.getValue();
	}

	public List<CervixHpvAnalyseresultaat> getAnalyseresultaten()
	{
		CervixHpvOrderCode orderCode = CervixHpvOrderCode.fromBerichtWaarde(getStringOrderCode());
		return List.of(
			new CervixHpvAnalyseresultaat(CervixHpvResultValue.fromValue(getStringResultValue(1)), CervixHpvResultCode.fromBerichtWaarde(getStringResultCode(1)), orderCode),
			new CervixHpvAnalyseresultaat(CervixHpvResultValue.fromValue(getStringResultValue(2)), CervixHpvResultCode.fromBerichtWaarde(getStringResultCode(2)), orderCode),
			new CervixHpvAnalyseresultaat(CervixHpvResultValue.fromValue(getStringResultValue(3)), CervixHpvResultCode.fromBerichtWaarde(getStringResultCode(3)), orderCode));
	}

	public boolean isFailure()
	{
		return getAnalyseresultaten().stream().anyMatch(ar -> CervixHpvResultValue.FAILURE.equals(ar.getResultValue()));
	}

	public String getAnalyseresultatenString()
	{
		String analyseresultatenString = "OrderCode: '" + getStringOrderCode() + "' ";
		boolean first = true;
		for (int i = 1; i <= 3; i++)
		{
			String resultCode = getStringResultCode(i);
			String resultValue = getStringResultValue(i);
			if (resultCode != null || resultValue != null)
			{
				if (first)
				{
					first = false;
				}
				else
				{
					analyseresultatenString += "; ";
				}
				analyseresultatenString +=
					"Result " + i + ": Code '" + StringUtils.defaultString(resultCode, "-leeg-") + "', Value '" + StringUtils.defaultString(resultValue, "-leeg-") + "'";
			}
		}
		return analyseresultatenString;
	}

	public boolean isValid()
	{
		List<CervixHpvAnalyseresultaat> analyseresultaten = getAnalyseresultaten();
		boolean sommigeAnalyseresultatenNietCompleet = analyseresultaten.stream()
			.anyMatch(ar -> ar.getResultValue() == null && ar.getResultCode() != null || ar.getResultValue() != null && ar.getResultCode() == null);
		List<CervixHpvAnalyseresultaat> completeAnalyseresultaten = getCompleteAnalyseresultaten(analyseresultaten);
		boolean heeftCompleteAnalyseresultaten = !completeAnalyseresultaten.isEmpty();
		boolean zijnAnalyseresultatenGoedAangeleverd = zijnAnalyseresultatenGoedAangeleverd(completeAnalyseresultaten);

		if (getBarcode() != null && getStringValueAnalyseDatum() != null && getStringValueAutorisatieDatum() != null
			&& !sommigeAnalyseresultatenNietCompleet && heeftCompleteAnalyseresultaten && zijnAnalyseresultatenGoedAangeleverd)
		{
			LOG.debug("--- Sample valide ---");
			LOG.debug("Barcode: {}", getBarcode());
			return true;
		}
		LOG.warn("--- Sample invalide ---");
		LOG.warn("Barcode: " + getBarcode());
		LOG.warn("AnalyseDatum: " + getStringValueAnalyseDatum());
		LOG.warn("AutorisatieDatum: " + getStringValueAutorisatieDatum());
		LOG.warn("Analyseresultaten: " + getAnalyseresultatenString());
		LOG.warn("sommigeAnalyseresultatenNietCompleet: " + sommigeAnalyseresultatenNietCompleet);
		LOG.warn("heeft complete analyseresultaten: " + heeftCompleteAnalyseresultaten);
		if (heeftCompleteAnalyseresultaten)
		{
			LOG.warn("analyseresultaten info consistent: " + zijnAnalyseresultatenGoedAangeleverd);
		}
		return false;
	}

	public static List<CervixHpvAnalyseresultaat> getCompleteAnalyseresultaten(List<CervixHpvAnalyseresultaat> analyseresultaten)
	{
		return analyseresultaten.stream().filter(ar -> ar.getResultValue() != null && ar.getResultCode() != null && ar.getOrderCode() != null).collect(Collectors.toList());
	}

	private static boolean zijnAnalyseresultatenGoedAangeleverd(List<CervixHpvAnalyseresultaat> completeAnalyseresultaten)
	{
		Set<CervixHpvResultCode> resultCodes = completeAnalyseresultaten.stream().map(ar -> ar.getResultCode()).collect(Collectors.toSet());
		Set<CervixHpvOrderCode> orderCodes = resultCodes.stream().map(rc -> rc.getOrderCode()).collect(Collectors.toSet());

		return orderCodes.size() == 1 &&
			(orderCodes.iterator().next().equals(CervixHpvOrderCode.GEN) && resultCodes.size() == 3 ||
				orderCodes.iterator().next().equals(CervixHpvOrderCode.PAN) && resultCodes.size() == 1) &&
			!completeAnalyseresultaten.stream().anyMatch(ar -> !ar.getResultValue().equals(CervixHpvResultValue.FAILURE)
				&& (!ar.getResultValue().getResultCode().equals(ar.getResultCode())
				|| !ar.getResultValue().getResultCode().getOrderCode().equals(ar.getOrderCode())));
	}

}
