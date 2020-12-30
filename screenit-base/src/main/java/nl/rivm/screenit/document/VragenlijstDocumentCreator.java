package nl.rivm.screenit.document;

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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierActieInstantie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;
import nl.topicuszorg.formulieren2.persistence.definitie.DefaultAntwoordKeuzeVraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.StringShowVraagActieInstantie;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;
import com.aspose.words.Document;

public class VragenlijstDocumentCreator
{

	private static final Logger LOG = LoggerFactory.getLogger(VragenlijstDocumentCreator.class);

	private int vraagNummer = 1;

	private DataTable vragen = null;

	private DataTable antwoorden = null;

	private DataSet dataSet;

	private Document document;

	private Map<String, StringShowVraagActieInstantie> acties = new HashMap<>();

	private Map<String, String> vraagTitels = new HashMap<>();

	public VragenlijstDocumentCreator(File vragenlijstTemplate, ScreenitFormulierInstantie vragenlijst)
	{
		initDataSet();
		processVragenlijst(vragenlijst);
		mergeDocument(vragenlijstTemplate);
	}

	private void initDataSet()
	{
		vragen = new DataTable("vragen");
		vragen.getColumns().add("id");
		vragen.getColumns().add("titel");
		vragen.getColumns().add("verplicht");
		vragen.getColumns().add("conditie");
		vragen.getColumns().add("tekst");
		vragen.getColumns().add("meervoudig");

		antwoorden = new DataTable("antwoorden");
		antwoorden.getColumns().add("vraagId");
		antwoorden.getColumns().add("vraagMarker");
		antwoorden.getColumns().add("tekst");

		dataSet = new DataSet();
		dataSet.getTables().add(vragen);
		dataSet.getTables().add(antwoorden);
		dataSet.getRelations().add(vragen, antwoorden, "id", "vraagId");
	}

	private void processVragenlijst(ScreenitFormulierInstantie vragenlijst)
	{
		try
		{
			for (FormulierActieInstantie<?, ?> actieInstantie : vragenlijst.getActies())
			{
				StringShowVraagActieInstantie actie = (StringShowVraagActieInstantie) actieInstantie;
				VraagDefinitie<?> vraagDefinitie = ((VraagInstantie<?>) actie.getTargetElement()).getVraagDefinitie();
				acties.put(((IdentifierElement) vraagDefinitie).getIdentifier(), actie);
			}

			processElementContainer((FormulierElementContainer) vragenlijst.getContainer());
		}
		catch (SQLException e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	private void processElementContainer(FormulierElementContainer<FormulierElement> elementContainer) throws SQLException
	{
		for (FormulierElement formulierElement : elementContainer.getElementen())
		{
			if (formulierElement instanceof FormulierElementContainer)
			{
				processElementContainer((FormulierElementContainer) formulierElement);
			}
			else if (formulierElement instanceof VraagInstantie)
			{
				processVraag((VraagInstantie<?>) formulierElement);
			}
		}
	}

	private void processVraag(VraagInstantie<?> vraag) throws SQLException
	{
		DefaultAntwoordKeuzeVraagDefinitieImpl<AntwoordDefinitie<?>> vraagDefinitie = (DefaultAntwoordKeuzeVraagDefinitieImpl<AntwoordDefinitie<?>>) vraag.getVraagDefinitie();

		String vraagId = ((IdentifierElement) vraagDefinitie).getIdentifier();
		String vraagTitel = "Vraag " + vraagNummer;
		vraagTitels.put(vraagId, vraagTitel);
		String vraagVerplicht = vraagDefinitie.getVerplichting() != null ? " *" : "";
		String vraagConditie = generateConditie(vraagDefinitie);
		String vraagTekst = vraagDefinitie.getVraag();
		String vraagMeervoudig;
		if (vraagDefinitie.getRenderType().equals(AntwoordRenderType.CHECKBOX_HORIZONTAAL)
			|| vraagDefinitie.getRenderType().equals(AntwoordRenderType.CHECKBOX_VERTICAAL))
		{
			vraagMeervoudig = "\nEr zijn meerdere antwoorden mogelijk.";
		}
		else
		{
			vraagMeervoudig = "";
		}

		vragen.getRows().add(vraagId, vraagTitel, vraagVerplicht, vraagConditie, vraagTekst, vraagMeervoudig);

		String vraagMarker = String.format("A+%02d", vraagNummer);
		for (AntwoordDefinitie<?> antwoordDefinitie : vraagDefinitie.getMogelijkeAntwoorden())
		{
			processAntwoord(vraagId, vraagMarker, antwoordDefinitie);
			vraagMarker = ""; 
		}

		vraagNummer++;
	}

	private String generateConditie(DefaultAntwoordKeuzeVraagDefinitieImpl<AntwoordDefinitie<?>> vraagDefinitie)
	{
		String vraagConditie = null;

		String vraagId = ((IdentifierElement) vraagDefinitie).getIdentifier();
		StringShowVraagActieInstantie vraagActie = acties.get(vraagId);
		if (vraagActie != null)
		{
			String afhankelijkheidTitel = vraagTitels.get(((IdentifierElement) vraagActie.getVraagInstantie().getVraagDefinitie()).getIdentifier());
			String afhankelijkheidAntwoord = vraagActie.getAntwoord();

			vraagConditie = "Beantwoord deze vraag indien " + afhankelijkheidTitel.toLowerCase() + " is ingevuld met \'" + afhankelijkheidAntwoord + "\'.\n";
		}
		else
		{
			vraagConditie = "";
		}
		return vraagConditie;
	}

	private void processAntwoord(String vraagId, String vraagMarker, AntwoordDefinitie<?> antwoordDefinitie) throws SQLException
	{
		String antwoordTekst = antwoordDefinitie.getAntwoordString();
		antwoorden.getRows().add(vraagId, vraagMarker, antwoordTekst);
	}

	private void mergeDocument(File vragenlijstTemplate)
	{
		try (InputStream stream = new FileInputStream(vragenlijstTemplate);)
		{
			document = new Document(stream);
			document.getMailMerge().executeWithRegions(vragen);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	public Document getDocument()
	{
		return document;
	}
}
