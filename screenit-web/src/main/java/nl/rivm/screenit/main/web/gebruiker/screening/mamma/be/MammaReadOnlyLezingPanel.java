package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.MammaVerslagRondePanel;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaReadOnlyLezingPanel extends GenericPanel<MammaBeoordeling>
{

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private final IModel<MammaLezing> lezingModel;

	public MammaReadOnlyLezingPanel(String id, final MammaBeoordeling beoordeling, MammaLezing lezing, boolean nevenbevindingenWeergeven,
		boolean redenenFotobesprekingWeergeven)
	{
		this(id, beoordeling, lezing, nevenbevindingenWeergeven, redenenFotobesprekingWeergeven, false);
	}

	public MammaReadOnlyLezingPanel(String id, final MammaBeoordeling beoordeling, MammaLezing lezing, boolean nevenbevindingenWeergeven,
		boolean redenenFotobesprekingWeergeven, boolean isVerslagenPagina)
	{
		super(id, ModelUtil.sModel(beoordeling));
		lezingModel = ModelUtil.ccModel(lezing);

		add(new Label("titelVerslagVerfijnen", lezing.getLezingType().getNaam()));

		var onderzoek = beoordeling.getOnderzoek();
		var lezingParameters = MammaLezingParameters.maakAlleenInzien()
			.setMetAfbeelding(true)
			.setAmputatie(onderzoek.getAmputatie())
			.setToonTomosyntheseSlicesRadioButtons(!isVerslagenPagina && MammaOnderzoekType.TOMOSYNTHESE == onderzoek.getOnderzoekType());

		var biradsPanel = new MammaLezingPanel(this, "birads", lezingModel, lezingParameters);

		add(biradsPanel);

		addNevenbevindingen(lezing, nevenbevindingenWeergeven);

		addFotobesprekingRedenen(lezing, redenenFotobesprekingWeergeven);

	}

	private void addNevenbevindingen(MammaLezing lezing, boolean nevenbevindingenWeergeven)
	{
		WebMarkupContainer nevenbevindingenContainer = new WebMarkupContainer("nevenbevindingenContainer");
		nevenbevindingenContainer.add(new Label("nevenbevindingen", baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, lezing)));

		String nevenbevindingOpmerkingTekst = baseBeoordelingService.getNevenbevindingOpmerkingTekst("<br />", lezing);
		Label nevenbevindingOpmerkingLabel = new Label("nevenbevindingenOpmerking", nevenbevindingOpmerkingTekst);
		nevenbevindingOpmerkingLabel.setEscapeModelStrings(false);
		nevenbevindingOpmerkingLabel.setVisible(nevenbevindingOpmerkingTekst != null);
		nevenbevindingenContainer.add(nevenbevindingOpmerkingLabel);
		nevenbevindingenContainer.setVisible(!lezing.getNevenbevindingen().isEmpty() && nevenbevindingenWeergeven);
		add(nevenbevindingenContainer);
	}

	private void addFotobesprekingRedenen(MammaLezing lezing, boolean redenenFotobesprekingWeergeven)
	{
		WebMarkupContainer redenenFotobesprekingRadioloogContainer = new WebMarkupContainer("redenenFotobesprekingRadioloogContainer");
		redenenFotobesprekingRadioloogContainer
			.add(new Label("redenenFotobesprekingRadioloog", baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getRedenenFotobesprekingRadioloog, lezing)));
		redenenFotobesprekingRadioloogContainer.setVisible(!lezing.getRedenenFotobesprekingRadioloog().isEmpty() && redenenFotobesprekingWeergeven);
		add(redenenFotobesprekingRadioloogContainer);

		WebMarkupContainer redenenFotobesprekingMbberContainer = new WebMarkupContainer("redenenFotobesprekingMbberContainer");
		redenenFotobesprekingMbberContainer
			.add(new Label("redenenFotobesprekingMbber", baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getRedenenFotobesprekingMbber, lezing)));
		redenenFotobesprekingMbberContainer.setVisible(!lezing.getRedenenFotobesprekingMbber().isEmpty() && redenenFotobesprekingWeergeven);
		add(redenenFotobesprekingMbberContainer);
	}

	public MammaVerslagRondePanel findMammaVerslagPanel()
	{
		MarkupContainer result = getParent();
		while (!(result instanceof MammaVerslagRondePanel))
		{
			result = result.getParent();
		}
		return (MammaVerslagRondePanel) result;
	}

	public IModel<MammaLezing> maakVerslagLezing()
	{
		InstellingGebruiker beoordelaar = ScreenitSession.get().getLoggedInInstellingGebruiker();
		MammaLezing verslagLezing = baseBeoordelingService.maakVerslagLezing(getModelObject(), lezingModel.getObject(), beoordelaar, isOnervarenRadioloog());
		return ModelUtil.cModel(verslagLezing);
	}

	private boolean isOnervarenRadioloog()
	{
		return !ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_ARBITRAGE_WERKLIJST, Actie.TOEVOEGEN);
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(lezingModel);
		super.onDetach();
	}
}
