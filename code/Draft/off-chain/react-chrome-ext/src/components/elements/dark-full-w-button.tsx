type Props = {
  clickHandler: (...args: any[]) => unknown;
  children: React.ReactNode;
};
function ButtonDarkFullWidth({ clickHandler, children }: Props) {
  return (
    <button
      onClick={clickHandler}
      className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
    >
      {children}
    </button>
  );
}

export default ButtonDarkFullWidth;
