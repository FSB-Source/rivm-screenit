package nl.rivm.screenit.model.cervix.berichten;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.cervix.enums.CervixHpvResultaatBerichtBron;

import org.apache.commons.lang3.StringUtils;

import ca.uhn.hl7v2.model.v251.datatype.DR;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;

@Slf4j
public class CervixHpvMonsterWrapper
{
	@Getter
	private final OUL_R22_SPECIMEN specimen;

	private final CervixHpvResultaatBerichtBron resultaatBerichtBron;

	public CervixHpvMonsterWrapper(OUL_R22_SPECIMEN specimen)
	{
		this.specimen = specimen;

		var order = specimen.getORDER();
		var result = order.getRESULT(0);
		var obx = result.getOBX();

		if (obx.getObx5_ObservationValue(0).getData() instanceof DR)
		{
			resultaatBerichtBron = CervixHpvResultaatBerichtBron.ROCHE;
		}
		else
		{
			resultaatBerichtBron = CervixHpvResultaatBerichtBron.LIMS;
		}
	}

	private static boolean zijnAnalyseresultatenGoedAangeleverd(List<CervixHpvAnalyseresultaat> completeAnalyseresultaten)
	{
		var resultCodes = completeAnalyseresultaten.stream().map(CervixHpvAnalyseresultaat::getResultCode).collect(Collectors.toSet());
		var orderCodes = resultCodes.stream().map(CervixHpvResultCode::getOrderCode).collect(Collectors.toSet());

		return orderCodes.size() == 1 &&
			(orderCodes.iterator().next().equals(CervixHpvOrderCode.GEN) && resultCodes.size() == 3 ||
				orderCodes.iterator().next().equals(CervixHpvOrderCode.PAN) && resultCodes.size() == 1) &&
			completeAnalyseresultaten.stream().noneMatch(ar -> !ar.getResultValue().equals(CervixHpvResultValue.FAILURE)
				&& (!ar.getResultValue().getResultCode().equals(ar.getResultCode())
				|| !ar.getResultValue().getResultCode().getOrderCode().equals(ar.getOrderCode())));
	}

	public String getBarcode()
	{
		var spm = specimen.getSPM();
		var spm2 = spm.getSpm2_SpecimenID().getEip1_PlacerAssignedIdentifier();
		var spm21 = spm2.getEi1_EntityIdentifier();
		return spm21.getValue();
	}

	public String getControleWaarde()
	{
		var container = specimen.getCONTAINER();
		var inv = container.getINV();
		var controle = inv.getInv1_SubstanceIdentifier();
		return controle.getCe1_Identifier().getValue();
	}

	private String getStringValueAnalyseDatum()
	{
		var order = specimen.getORDER();

		if (CervixHpvResultaatBerichtBron.ROCHE == resultaatBerichtBron)
		{
			var result = order.getRESULT(0);
			var obx = result.getOBX();

			var data = (DR) obx.getObx5_ObservationValue(0).getData();
			return data.getDr1_RangeStartDateTime().getTs1_Time().getValue();
		}
		else
		{
			return order.getOBR().getObr7_ObservationDateTime().getTs1_Time().getValue();
		}
	}

	public Date getAnalyseDatum()
	{
		Date analyseDatum = null;
		try
		{
			var datum = getStringValueAnalyseDatum();
			var format = new SimpleDateFormat("yyyyMMddhhmmss");
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
		var order = specimen.getORDER();

		if (CervixHpvResultaatBerichtBron.ROCHE == resultaatBerichtBron)
		{
			var result = order.getRESULT(0);
			var obx = result.getOBX();

			var data = (DR) obx.getObx5_ObservationValue(0).getData();
			return data.getDr2_RangeEndDateTime().getTs1_Time().getValue();
		}
		else
		{
			return order.getOBR().getObr22_ResultsRptStatusChngDateTime().getTs1_Time().getValue();
		}
	}

	public Date getAutorisatieDatum()
	{
		Date autorisatieDatum = null;
		try
		{
			var datum = getStringValueAutorisatieDatum();
			var format = new SimpleDateFormat("yyyyMMddhhmmss");
			autorisatieDatum = format.parse(datum);
		}
		catch (ParseException e)
		{
			return null;
		}
		return autorisatieDatum;
	}

	private String getStringResultValue(int repetitionIndex)
	{
		var order = specimen.getORDER();
		if (order.getRESULTReps() > repetitionIndex)
		{
			var result = order.getRESULT(repetitionIndex);
			var obx = result.getOBX();
			var data = obx.getObx5_ObservationValue(0).getData();
			if (data instanceof ST)
			{
				return ((ST) data).getValue();
			}
		}
		return null;
	}

	private String getStringResultCode(int repetitionIndex)
	{
		var order = specimen.getORDER();
		if (order.getRESULTReps() > repetitionIndex)
		{
			var result = order.getRESULT(repetitionIndex);
			var obx = result.getOBX();
			var st = obx.getObx3_ObservationIdentifier().getCe1_Identifier();
			return st.getValue();
		}
		else
		{
			return null;
		}
	}

	private String getStringOrderCode()
	{
		var order = specimen.getORDER();
		var st = order.getOBR().getObr4_UniversalServiceIdentifier().getCe1_Identifier();
		return st.getValue();
	}

	public List<CervixHpvAnalyseresultaat> getAnalyseresultaten()
	{
		var orderCode = CervixHpvOrderCode.fromBerichtWaarde(getStringOrderCode());

		var startRepetitionIndex = CervixHpvResultaatBerichtBron.ROCHE == resultaatBerichtBron ? 1 : 0;

		return IntStream
			.range(startRepetitionIndex, startRepetitionIndex + 3)
			.mapToObj(repetitionIndex -> new CervixHpvAnalyseresultaat(CervixHpvResultValue.fromValue(getStringResultValue(repetitionIndex)),
				CervixHpvResultCode.fromBerichtWaarde(getStringResultCode(repetitionIndex)), orderCode))
			.collect(Collectors.toList());
	}

	public boolean isFailure()
	{
		return getAnalyseresultaten().stream().anyMatch(ar -> CervixHpvResultValue.FAILURE.equals(ar.getResultValue()));
	}

	public static List<CervixHpvAnalyseresultaat> getCompleteAnalyseresultaten(List<CervixHpvAnalyseresultaat> analyseresultaten)
	{
		return analyseresultaten.stream().filter(ar -> ar.getResultValue() != null && ar.getResultCode() != null && ar.getOrderCode() != null).collect(Collectors.toList());
	}

	public String getAnalyseresultatenString()
	{
		var startRepetitionIndex = CervixHpvResultaatBerichtBron.ROCHE == resultaatBerichtBron ? 1 : 0;

		var analyseresultatenString = "OrderCode: '" + getStringOrderCode() + "' ";
		var first = true;
		for (var i = startRepetitionIndex; i <= startRepetitionIndex + 2; i++)
		{
			var resultCode = getStringResultCode(i);
			var resultValue = getStringResultValue(i);
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
		var analyseresultaten = getAnalyseresultaten();
		var sommigeAnalyseresultatenNietCompleet = analyseresultaten.stream()
			.anyMatch(ar -> ar.getResultValue() == null && ar.getResultCode() != null || ar.getResultValue() != null && ar.getResultCode() == null);
		var completeAnalyseresultaten = getCompleteAnalyseresultaten(analyseresultaten);
		var heeftCompleteAnalyseresultaten = !completeAnalyseresultaten.isEmpty();
		var zijnAnalyseresultatenGoedAangeleverd = zijnAnalyseresultatenGoedAangeleverd(completeAnalyseresultaten);

		if (getBarcode() != null && getStringValueAnalyseDatum() != null && getStringValueAutorisatieDatum() != null
			&& !sommigeAnalyseresultatenNietCompleet && heeftCompleteAnalyseresultaten && zijnAnalyseresultatenGoedAangeleverd)
		{
			return true;
		}
		LOG.warn("--- Sample invalide ---");
		LOG.warn("Bron resultaat: " + resultaatBerichtBron);
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

}
