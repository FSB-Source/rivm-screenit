package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.mamma.MammaBasePaVerslagService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientVerslagenPanel extends GenericPanel<Client>
{

	private Boolean bezwaarOpIntake;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private VerwerkVerslagService verwerkVerslagService;

	@SpringBean
	private MammaBasePaVerslagService basePaVerslagService;

	public ClientVerslagenPanel(String id, IModel<Client> model)
	{
		super(id, model);

		add(new ClientPaspoortPanel("passpoort", model));

		Client client = model.getObject();
		boolean magMdlVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.TOEVOEGEN, client);
		boolean magPaVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.TOEVOEGEN, client);
		boolean magFollowUpPaVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.TOEVOEGEN, client)
			&& basePaVerslagService.verwachtGegevensVoor(model.getObject().getPersoon().getBsn());

		if (getLaatsteScreeningronde(VerslagType.MDL) == null)
		{
			magMdlVerslagToevoegen = magPaVerslagToevoegen = false;
		}

		boolean magToevoegen = magMdlVerslagToevoegen || magPaVerslagToevoegen || magFollowUpPaVerslagToevoegen;
		bezwaarOpIntake = clientService.heeftClientIntakeConclusieMetBezwaar(client.getPersoon().getBsn());

		WebMarkupContainer toevoegen = new WebMarkupContainer("toevoegen");
		toevoegen.setVisible(magToevoegen);
		toevoegen.add(new IndicatingAjaxLink<Object>("mdl")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MdlVerslag newVerslag = new MdlVerslag();
				newVerslag.setStatus(VerslagStatus.IN_BEWERKING);
				MdlVerslagContent content = new MdlVerslagContent();
				newVerslag.setVerslagContent(content);
				newVerslag.setType(VerslagType.MDL);
				content.setVerslag(newVerslag);
				content.setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.MDL));
				newVerslag.setScreeningRonde(getLaatsteScreeningronde(VerslagType.MDL));
				if (bezwaarOpIntake.equals(Boolean.TRUE))
				{
					info("Cliënt heeft bezwaar gemaakt tegen gegevensuitwisseling. Vastleggen van een verslag is niet mogelijk");
				}
				else
				{
					setResponsePage(new ClientVerslagPage(ModelUtil.dModel(newVerslag)));
				}
			}

		}.setVisible(magMdlVerslagToevoegen));
		toevoegen.add(new IndicatingAjaxLink<Object>("pa")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				PaVerslag newVerslag = new PaVerslag();
				newVerslag.setStatus(VerslagStatus.IN_BEWERKING);
				PaVerslagContent content = new PaVerslagContent();
				newVerslag.setVerslagContent(content);
				newVerslag.setType(VerslagType.PA_LAB);
				content.setVerslag(newVerslag);
				content.setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.PA_LAB));
				newVerslag.setScreeningRonde(getLaatsteScreeningronde(VerslagType.PA_LAB));
				if (bezwaarOpIntake.equals(Boolean.TRUE))
				{
					info("Cliënt heeft bezwaar gemaakt tegen gegevensuitwisseling. Vastleggen van een verslag is niet mogelijk");
				}
				else
				{
					setResponsePage(new ClientVerslagPage(ModelUtil.dModel(newVerslag)));
				}
			}

		}.setVisible(magPaVerslagToevoegen));
		add(toevoegen);
		toevoegen.add(new IndicatingAjaxLink<Object>("follow-up")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaFollowUpVerslag newVerslag = new MammaFollowUpVerslag();
				newVerslag.setStatus(VerslagStatus.IN_BEWERKING);
				MammaFollowUpVerslagContent content = new MammaFollowUpVerslagContent();
				newVerslag.setVerslagContent(content);
				newVerslag.setType(VerslagType.MAMMA_PA_FOLLOW_UP);
				content.setVerslag(newVerslag);
				content.setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.MAMMA_PA_FOLLOW_UP));
				newVerslag.setScreeningRonde(getLaatsteScreeningronde(VerslagType.MAMMA_PA_FOLLOW_UP));
				if (bezwaarOpIntake.equals(Boolean.TRUE))
				{
					info("Cliënt heeft bezwaar gemaakt tegen gegevensuitwisseling. Vastleggen van een verslag is niet mogelijk");
				}
				else
				{
					setResponsePage(new ClientVerslagPage(ModelUtil.dModel(newVerslag)));
				}
			}

		}.setVisible(magFollowUpPaVerslagToevoegen));

		ColonClientVerslagenOverzichtPanel colonVerslagen = new ColonClientVerslagenOverzichtPanel("colonVerslagen", model);
		add(colonVerslagen);
		CervixClientVerslagenOverzichtPanel cervixVerslagen = new CervixClientVerslagenOverzichtPanel("cervixVerslagen", model);
		add(cervixVerslagen);
		MammaClientVerslagenOverzichtPanel mammaVerslagen = new MammaClientVerslagenOverzichtPanel("mammaVerslagen", model);
		add(mammaVerslagen);

	}

	private <T extends ScreeningRonde> T getLaatsteScreeningronde(VerslagType verslagType)
	{
		return (T) HibernateHelper.deproxy(verwerkVerslagService.getValideScreeningsRonde(verslagType, getModelObject(), null, null));
	}

}
