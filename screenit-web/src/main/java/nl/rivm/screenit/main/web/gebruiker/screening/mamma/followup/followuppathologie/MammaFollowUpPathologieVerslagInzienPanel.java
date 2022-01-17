package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpFollowupPa;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpMonstermateriaal;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPtnmEnGradering;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaFollowUpPathologieVerslagInzienPanel extends GenericPanel<MammaFollowUpVerslag>
{
	public MammaFollowUpPathologieVerslagInzienPanel(String id, IModel<MammaFollowUpVerslag> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(DateLabel.forDatePattern("verslagContent.pathologieMedischeObservatie.datumOntvangstMateriaal", Constants.DEFAULT_DATE_FORMAT));
		add(new Label("verslagContent.pathologieMedischeObservatie.tnummerLaboratorium"));
		add(DateLabel.forDatePattern("verslagContent.pathologieMedischeObservatie.datumAutorisatieUitslag", Constants.DEFAULT_DATE_FORMAT));

		add(new Label("invoermethode", Model.of(getString(getInvoermethode()))));

		add(DateLabel.forDatePattern("verslagContent.verrichting.eindeVerrichting", Constants.DEFAULT_DATE_FORMAT));

		ListView<MammaFollowUpFollowupPa> followUpPa = new ListView<>("verslagContent.followupPa")
		{
			@Override
			protected void populateItem(ListItem<MammaFollowUpFollowupPa> followUpPaListItem)
			{
				MammaFollowUpFollowupPa verslag = followUpPaListItem.getModelObject();
				WebMarkupContainer table = new WebMarkupContainer("table", new CompoundPropertyModel<>(followUpPaListItem.getModel()));

				MammaFollowUpMonstermateriaal monstermateriaal = verslag.getMonstermateriaal();
				boolean verkrijgingsWijze = monstermateriaal != null && monstermateriaal.getVerkrijgingswijze() != null;
				boolean locatietopologie = monstermateriaal != null && monstermateriaal.getLocatietopologie() != null;
				boolean locatieUren = monstermateriaal != null && monstermateriaal.getLocatieuren() != null;
				boolean zijdigheid = monstermateriaal != null && monstermateriaal.getZijdigheid() != null;
				boolean cclassificatiePuntie = verslag.getCclassificatiePunctie() != null;
				boolean oestrogeenReceptorStatus = verslag.getOestrogeenReceptorStatus() != null;
				boolean progesteronReceptorStatus = verslag.getProgesteronReceptorStatus() != null;
				boolean her2Status = verslag.getHer2Status() != null;
				boolean bclassificatieOpMammabiopt = verslag.getBclassificatieOpMammabiopt() != null;
				boolean maligniteitsgraad = verslag.getMaligniteitsgraad() != null;
				MammaFollowUpPtnmEnGradering ptnmEnGradering = verslag.getPtnmEnGradering();
				boolean ptnmbreastGradering = ptnmEnGradering != null && ptnmEnGradering.getPtnmbreastGradering() != null;
				boolean pt = ptnmEnGradering != null && ptnmEnGradering.getPt() != null;
				boolean pn = ptnmEnGradering != null && ptnmEnGradering.getPn() != null;
				boolean pm = ptnmEnGradering != null && ptnmEnGradering.getPm() != null;

				table.add(new WebMarkupContainer("headerVerkrijgingswijze").setVisible(verkrijgingsWijze));
				table.add(new WebMarkupContainer("headerLocatietopologie").setVisible(locatietopologie));
				table.add(new WebMarkupContainer("headerLocatieuren").setVisible(locatieUren));
				table.add(new WebMarkupContainer("headerZijdigheid").setVisible(zijdigheid));
				table.add(new WebMarkupContainer("headerCclassificatiePunctie").setVisible(cclassificatiePuntie));
				table.add(new WebMarkupContainer("headerOestrogeenReceptorStatus").setVisible(oestrogeenReceptorStatus));
				table.add(new WebMarkupContainer("headerProgesteronReceptorStatus").setVisible(progesteronReceptorStatus));
				table.add(new WebMarkupContainer("headerHer2Status").setVisible(her2Status));
				table.add(new WebMarkupContainer("headerBclassificatieOpMammabiopt").setVisible(bclassificatieOpMammabiopt));
				table.add(new WebMarkupContainer("headerMaligniteitsgraad").setVisible(maligniteitsgraad));
				table.add(new WebMarkupContainer("headerPtnmbreastGradering").setVisible(ptnmbreastGradering));
				table.add(new WebMarkupContainer("headerPt").setVisible(pt));
				table.add(new WebMarkupContainer("headerPn").setVisible(pn));
				table.add(new WebMarkupContainer("headerPm").setVisible(pm));

				table.add(new Label("monstermateriaal.verkrijgingswijze.displayNameNl").setVisible(verkrijgingsWijze));
				table.add(new Label("monstermateriaal.locatietopologie.displayNameNl").setVisible(locatietopologie));
				table.add(new Label("monstermateriaal.locatieuren.displayNameNl").setVisible(locatieUren));
				table.add(new Label("monstermateriaal.zijdigheid.displayNameNl").setVisible(zijdigheid));
				table.add(new Label("cclassificatiePunctie.displayNameNl").setVisible(cclassificatiePuntie));
				table.add(new Label("oestrogeenReceptorStatus.displayNameNl").setVisible(oestrogeenReceptorStatus));
				table.add(new Label("progesteronReceptorStatus.displayNameNl").setVisible(progesteronReceptorStatus));
				table.add(new Label("her2Status.displayNameNl").setVisible(her2Status));
				table.add(new Label("bclassificatieOpMammabiopt.displayNameNl").setVisible(bclassificatieOpMammabiopt));
				table.add(new Label("maligniteitsgraad.displayNameNl").setVisible(maligniteitsgraad));
				table.add(new Label("ptnmEnGradering.ptnmbreastGradering.displayNameNl").setVisible(ptnmbreastGradering));
				table.add(new Label("ptnmEnGradering.pt.displayNameNl").setVisible(pt));
				table.add(new Label("ptnmEnGradering.pn.displayNameNl").setVisible(pn));
				table.add(new Label("ptnmEnGradering.pm.displayNameNl").setVisible(pm));
				followUpPaListItem.add(table);
			}

		};
		add(followUpPa);
	}

	private String getInvoermethode()
	{
		MammaFollowUpVerslag followUpVerslag = getModelObject();
		if (followUpVerslag != null && followUpVerslag.getInvoerder() == null)
		{
			return followUpVerslag.getVerslagContent().getPathologieMedischeObservatie().getVersieProtocol() != null ? "protocollair" : "niet.protocollair";
		}
		else
		{
			return "handmatig";
		}
	}

}
