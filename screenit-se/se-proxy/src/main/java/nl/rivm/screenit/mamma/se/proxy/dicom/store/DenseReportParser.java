package nl.rivm.screenit.mamma.se.proxy.dicom.store;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.model.DenseWaarde;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;

@Slf4j
public class DenseReportParser
{
	private static final String DICOM_SCHEME = "DCM";

	private static final String SNOMED_RT_SCHEME = "SRT";

	private static final String QUANTRA_BREAST_DENSITY_CATEGORIES_SCHEME = "99R2TECH";

	private static final String CAD_PROCESSING_AND_FINDINGS_SUMMARY_CODE = "111017";

	private static final String INDIVIDUAL_IMPRESSION_RECOMMENDATION_CODE = "111034";

	private static final String QUANTRA_BREAST_DENSITY_CATEGORIES_CODE = "R2cn027";

	private static final String CALCULATION_DESCRIPTION_CODE = "112034";

	private static final String LATERALITY_CODE = "G-C171";

	private static final String BOTH_BREASTS_CODE = "T-04080";

	private final Attributes structuredReport;

	public DenseReportParser(Attributes structuredReport)
	{
		this.structuredReport = structuredReport;
	}

	public Optional<Long> getAccessionNumber()
	{
		var accessionNumber = structuredReport.getString(Tag.AccessionNumber);
		try
		{
			return Optional.of(Long.parseLong(accessionNumber));
		}
		catch (NumberFormatException e)
		{
			LOG.warn("Niet-numeriek accessionnummer in structured report: '{}'", accessionNumber);
			return Optional.empty();
		}
	}

	public Optional<DenseWaarde> getDenseValueBothBreasts()
	{
		try
		{

			var processingAndFindingSummary = getFirstContentItemWithConcept(structuredReport, CAD_PROCESSING_AND_FINDINGS_SUMMARY_CODE);
			var individualImpressionRecommendation = getFirstContentItemWithConcept(processingAndFindingSummary, INDIVIDUAL_IMPRESSION_RECOMMENDATION_CODE);
			var quantraBreastDensityCategories = getContentItemsWithConcept(individualImpressionRecommendation, QUANTRA_BREAST_DENSITY_CATEGORIES_CODE,
				QUANTRA_BREAST_DENSITY_CATEGORIES_SCHEME);
			var bothBreastsItem = quantraBreastDensityCategories.stream().filter(this::isBothBreastsLaterality).findFirst().orElseThrow();
			var calculationDescription = getFirstContentItemWithConcept(bothBreastsItem, CALCULATION_DESCRIPTION_CODE);
			var ruweDenseWaarde = calculationDescription.getString(Tag.TextValue);
			return parseDenseWaarde(ruweDenseWaarde);
		}
		catch (Exception e)
		{
			LOG.warn("Kan densewaarde niet vinden in structured report", e);
			return Optional.empty();
		}
	}

	private Attributes getFirstContentItemWithConcept(Attributes root, String code)
	{
		return getContentItemsWithConcept(root, code, DICOM_SCHEME).get(0);
	}

	private List<Attributes> getContentItemsWithConcept(Attributes root, String code, String scheme)
	{
		return root.getSequence(Tag.ContentSequence).stream().filter(item -> hasConceptName(item, code, scheme)).collect(Collectors.toList());
	}

	private boolean hasConceptName(Attributes contentitem, String code, String scheme)
	{
		var conceptNameCode = contentitem.getSequence(Tag.ConceptNameCodeSequence).get(0);
		return scheme.equals(conceptNameCode.getString(Tag.CodingSchemeDesignator)) && code.equals(conceptNameCode.getString(Tag.CodeValue));
	}

	private boolean isBothBreastsLaterality(Attributes root)
	{
		var lateralityItems = getContentItemsWithConcept(root, LATERALITY_CODE, SNOMED_RT_SCHEME);
		if (lateralityItems.size() != 1)
		{
			throw new IllegalStateException("Meerdere laterality items gevonden op enkel nivo");
		}
		return matchesBothBreastsConceptCode(lateralityItems.get(0));
	}

	private boolean matchesBothBreastsConceptCode(Attributes item)
	{
		var codeSequence = item.getSequence(Tag.ConceptCodeSequence).get(0);
		return BOTH_BREASTS_CODE.equals(codeSequence.getString(Tag.CodeValue)) && SNOMED_RT_SCHEME.equals(codeSequence.getString(Tag.CodingSchemeDesignator));
	}

	private Optional<DenseWaarde> parseDenseWaarde(String ruweDenseWaarde)
	{
		if (ruweDenseWaarde != null && ruweDenseWaarde.toLowerCase().matches("[abcd]"))
		{
			if (ruweDenseWaarde.equalsIgnoreCase("d"))
			{
				return Optional.of(DenseWaarde.D);
			}
			return Optional.of(DenseWaarde.KLEINER_DAN_D);
		}
		LOG.warn("Ongeldige densewaarde gevonden: '{}'", ruweDenseWaarde);
		return Optional.empty();
	}
}
