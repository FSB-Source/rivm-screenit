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
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.comparator.MammaLaesieComparator;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;
import com.aspose.words.MailMerge;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;

public class MammaVerslagDocumentCreator extends BaseDocumentCreator
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaVerslagDocumentCreator.class);

	private final MammaBaseAfbeeldingService afbeeldingService;

	private final DataSet verslagDataset = new DataSet();

	private final MammaLezing verslagLezing;

	private final MammaBaseLaesieService laesieService;

	private final MammaBaseLezingService baseLezingService;

	public MammaVerslagDocumentCreator(MammaLezing verslagLezing)
	{
		afbeeldingService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseAfbeeldingService.class);
		laesieService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseLaesieService.class);
		baseLezingService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseLezingService.class);
		this.verslagLezing = (MammaLezing) HibernateHelper.deproxy(verslagLezing);
		if (verslagLezing != null)
		{
			createLaesieTables();
			createTomosyntheseTable();
		}
	}

	private void createTomosyntheseTable()
	{
		var afwijkingTeZienOp = verslagLezing.getAfwijkingTeZienOp();
		if (afwijkingTeZienOp != null)
		{
			var formattedZijde = StringUtils.capitalize(baseLezingService.bepaalZijdeMetPrioriteit(verslagLezing).getNaam());
			var tomosyntheseTabelMerge = "tomosynthese";
			getOrCreateDataTable(verslagDataset, tomosyntheseTabelMerge, "_BK_LAESIE_VIEW", "_BK_LAESIE_SLICE_NR", "_BK_LAESIE_ZIJDE");
			var tomoDataTable = getDataTable(verslagDataset, tomosyntheseTabelMerge);
			switch (afwijkingTeZienOp)
			{
			case C_VIEW_EN_SLICES:
				insertRow(tomoDataTable, "C-view en slices", "", formattedZijde);
				break;
			case SLICES_CC_EN_MLO:
				insertRow(tomoDataTable, "Slices CC & MLO", String.format("CC %s & MLO %s", verslagLezing.getZichtbaarOpCcNummer(), verslagLezing.getZichtbaarOpMloNummer()),
					formattedZijde);
				break;
			case ALLEEN_SLICE_CC:
				insertRow(tomoDataTable, "Alleen slice CC", String.format("CC %s", verslagLezing.getZichtbaarOpCcNummer()), formattedZijde);
				break;
			case ALLEEN_SLICE_MLO:
				insertRow(tomoDataTable, "Alleen slice MLO", String.format("MLO %s", verslagLezing.getZichtbaarOpMloNummer()), formattedZijde);
				break;
			default:
				throw new IllegalStateException();
			}
		}
	}

	private void createLaesieTables()
	{
		verslagLezing.getLaesies().stream()
			.sorted(new MammaLaesieComparator())
			.forEachOrdered(laesie ->
			{
				MammaLaesieType laesieType = laesie.getMammaLaesieType();
				switch (laesieType)
				{
				case MASSA:
					createMassaLaesieTabel((MammaMassaLaesie) laesie);
					break;
				case CALCIFICATIES:
					createCalcificatieLaesieTabel((MammaCalcificatiesLaesie) laesie);
					break;
				case ARCHITECTUURVERSTORING:
					createArchitectuurVerstoringLaesieTabel((MammaArchitectuurverstoringLaesie) laesie);
					break;
				case ASYMMETRIE:
					createAsymmetrieLaesieTabel((MammaAsymmetrieLaesie) laesie);
					break;
				default:
				}
			});
	}

	private void createAsymmetrieLaesieTabel(MammaAsymmetrieLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = laesieService.getAsymetrieLaesieMap(laesie);
		insertRow(createAsymmetrieTabel(),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_ASSYMETRIE_SPEC));
	}

	private void createArchitectuurVerstoringLaesieTabel(MammaArchitectuurverstoringLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = laesieService.getArchitectuurVerstoringMap(laesie);
		insertRow(createArchitectuurVerstoringTabel(),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE));
	}

	private void createCalcificatieLaesieTabel(MammaCalcificatiesLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = laesieService.getCalificatiesMap(laesie);
		insertRow(createCalcificatiesTabel(), laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_CALC_VERD_VORM),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_CALC_DISTRIBUTIE));
	}

	private void createMassaLaesieTabel(MammaMassaLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap;
		laesieMap = laesieService.getMassaMap(laesie);
		insertRow(createMassaTabel(), laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR), laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE), laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_VORM), laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_DENSITEIT),
			laesieMap.get(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_BEGRENZING));
	}

	private DataTable createMassaTabel()
	{
		getOrCreateDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_MASSA.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_MASSA_VORM.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_MASSA_DENSITEIT.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_MASSA_BEGRENZING.getMergeField());
		return getDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_MASSA.getMergeField());
	}

	private DataTable createCalcificatiesTabel()
	{
		getOrCreateDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_CALCIFICATIES.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_CALC_VERD_VORM.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_CALC_DISTRIBUTIE.getMergeField());
		return getDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_CALCIFICATIES.getMergeField());
	}

	private DataTable createArchitectuurVerstoringTabel()
	{
		getOrCreateDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_ARCHITECTUUR_VERSTORING.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE.getMergeField());
		return getDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_ARCHITECTUUR_VERSTORING.getMergeField());
	}

	private DataTable createAsymmetrieTabel()
	{
		getOrCreateDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_ASYMMETRIE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE.getMergeField(),
			MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE.getMergeField(), MammaLaesieTypeMergeField._BK_LAESIE_ASSYMETRIE_SPEC.getMergeField());
		return getDataTable(verslagDataset, MammaLaesieTypeMergeField.TABLE_ASYMMETRIE.getMergeField());
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		log(LOG, verslagDataset);
		MailMerge mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(verslagDataset);

		String[] velden = new String[] {
			MammaLaesieTypeMergeField._AFBEELDING_RECHTERBORST_VERTICALE_DOORSNEDE.getMergeField(),
			MammaLaesieTypeMergeField._AFBEELDING_LINKERBORST_VERTICALE_DOORSNEDE.getMergeField(),
			MammaLaesieTypeMergeField._AFBEELDING_RECHTERBORST_HORIZONTALE_DOORSNEDE.getMergeField(),
			MammaLaesieTypeMergeField._AFBEELDING_LINKERBORST_HORIZONTALE_DOORSNEDE.getMergeField(), };

		if (verslagLezing != null)
		{
			List<InputStream> afbeeldingen = afbeeldingService.createLaesiesAfbeeldingen(verslagLezing, verslagLezing.getBeoordeling().getOnderzoek().getAmputatie());
			Object[] inhoud = afbeeldingen.toArray();

			mailMerge.setFieldMergingCallback(new MailMergeImageCallback());
			mailMerge.execute(velden, inhoud);
		}

		return document;
	}

}
