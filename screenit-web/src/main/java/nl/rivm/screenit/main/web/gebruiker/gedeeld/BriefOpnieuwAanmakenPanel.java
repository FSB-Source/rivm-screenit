package nl.rivm.screenit.main.web.gebruiker.gedeeld;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.util.GebeurtenisUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.util.BriefUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

@Slf4j
public class BriefOpnieuwAanmakenPanel extends GenericPanel<ClientBrief<?, ?, ?>>
{
	@SpringBean
	private BriefHerdrukkenService briefHerdrukkenService;

	public BriefOpnieuwAanmakenPanel(String id, IModel<ClientBrief<?, ?, ?>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		Date datum = BriefUtil.geefDatumVoorGebeurtenisoverzicht(getModelObject());
		maakBriefOpnieuwAanmakenContent(getModelObject(), datum);
	}

	private void maakBriefOpnieuwAanmakenContent(ClientBrief<?, ?, ?> brief, Date datum)
	{
		WebMarkupContainer opnieuwContainer = new WebMarkupContainer("opnieuwContainer");
		opnieuwContainer.setOutputMarkupId(true);
		opnieuwContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN, Actie.AANPASSEN));
		WebMarkupContainer opnieuwMogelijkContainer = new WebMarkupContainer("opnieuwMogelijk");

		GebeurtenisUtil.voegBriefTypeOfNaamBriefToe(opnieuwMogelijkContainer, brief);

		boolean magOpnieuwAanvragen = briefHerdrukkenService.magHerdrukken(brief);

		WebMarkupContainer nietOpnieuw = new WebMarkupContainer("nietOpnieuw");
		nietOpnieuw.setOutputMarkupId(true);

		nietOpnieuw.add(new Label("tekstNietOpnieuw", Model.of(geefBriefNietOpnieuwAanmakenTekst(brief))));
		nietOpnieuw.setVisible(!magOpnieuwAanvragen);

		opnieuwMogelijkContainer.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(datum), "dd-MM-yyyy"));
		opnieuwMogelijkContainer.setOutputMarkupId(true);
		opnieuwMogelijkContainer.add(new IndicatingAjaxLink<Void>("aanmaken")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				briefHerdrukkenService.opnieuwAanmaken(BriefOpnieuwAanmakenPanel.this.getModelObject(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefaangemaakt"));
				opnieuwMogelijkContainer.setVisible(false);
				nietOpnieuw.setVisible(true);
				target.add(opnieuwContainer);
			}
		});

		opnieuwMogelijkContainer.setVisible(magOpnieuwAanvragen);

		opnieuwContainer.add(opnieuwMogelijkContainer);

		opnieuwContainer.add(nietOpnieuw);
		add(opnieuwContainer);
	}

	private String geefBriefNietOpnieuwAanmakenTekst(ClientBrief<?, ?, ?> brief)
	{
		String melding;
		if (BriefUtil.isHerdruk(brief))
		{
			melding = getString("message.briefisaleenherdruk");
		}
		else if (BriefType.getCervixUitnodigingen().contains(brief.getBriefType()) || BriefType.getCervixZasBrieven().contains(brief.getBriefType()))
		{
			melding = getString("message.cervixuitnodiging");
		}
		else
		{
			melding = getString("message.nietmogelijkbriefopnieuw");
		}

		return melding;
	}
}
